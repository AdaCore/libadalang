import argparse
import collections
import subprocess as sp
import os
import os.path as path
import sys
import tempfile
import traceback
import xml.etree.ElementTree as etree

import pygments
import pygments.formatters
import pygments.style
from pygments.token import *


#
# Main testsuite framework
#


def match_testcase(test_patterns, testcase):
    """
    Return whether `testcase` matches at least one pattern.  Always return true
    when there is no test pattern.
    """
    return (
        not test_patterns
        or any(testcase.startswith(pattern) for pattern in test_patterns)
    )


class Stats(object):
    """Helper to hold statistics about run tests."""

    PASSED, FAILED, REWRITTEN, CRASHED = range(4)

    COLUMNS = (PASSED, FAILED, REWRITTEN, CRASHED)
    COLUMNS_LABEL = {
        PASSED:    (Generic.OK,        'OK'),
        FAILED:    (Generic.Error,     'FAIL'),
        REWRITTEN: (Generic.Rewritten, 'Rewritten'),
        CRASHED:   (Generic.Crash,     'CRASH'),
    }
    LABEL_MAX_LENGTH = max(
        len(label[1])
        for label in COLUMNS_LABEL.itervalues()
    )

    def __init__(self):
        self.cols = {key: 0 for key in self.COLUMNS}

    def add(self, status):
        self.cols[status] += 1

    def label(self, status):
        return self.COLUMNS_LABEL[status]

    def label_padding(self, status):
        token, text = self.label(status)
        return (Text, ' ' * (self.LABEL_MAX_LENGTH - len(text)))

    def __iter__(self):
        for column in self.COLUMNS:
            yield column, self.cols[column]


class Testcase(object):
    def __init__(self, name):
        self.name = name

    @property
    def input_file(self):
        return path.join(self.name, 'input')

    @property
    def expected_file(self):
        return path.join(self.name, 'expected')


class Testsuite(object):

    def __init__(self, console, run_valgrind, debug, debugger, rewrite):
        self.run_valgrind = run_valgrind
        self.debug = debug
        self.debugger = debugger
        self.rewrite = rewrite

        self.console = console
        self.memory_errors = MemoryErrors()
        self.stats = Stats()

        self.queue = collections.deque()

    def add_test(self, testcase):
        self.queue.append(testcase)

    def run(self):
        while self.queue:
            testcase = self.queue.popleft()
            try:
                self._run_testcase(testcase)
            except Exception as e:
                self.report_testcase(testcase, Stats.CRASHED)
                self.console.write((Text, traceback.format_exc()))
                self.console.endl()

    def report_testcase(self, testcase, status):
        label = self.stats.label(status)
        self.stats.add(status)
        self.console.write(
            # Padding to align labels to the right
            self.stats.label_padding(status),
            label,
            (Text, ' '),
            (Text, testcase.name)
        )
        self.console.endl()

    def _run_testcase(self, testcase):
        with open(testcase.input_file) as f:
            # Read the rule to use to start parsing.
            rule_name = f.readline().strip()

            # Read AST node lookups to process.
            lookups = []
            while True:
                pos = f.tell()
                lookup_line = f.readline().strip()
                if lookup_line.startswith("lookup: "):
                    _, sloc = lookup_line.split()
                    line, column = sloc.split(":")
                    lookups.append((int(line), int(column)))
                else:
                    f.seek(pos)
                    break

            # The rest of the file is the text to parse.
            input_text = f.read().strip()

        parse_argv = [
            "../build/bin/parse",
            "-r", rule_name,
            "--input", input_text,
        ]
        for line, column in lookups:
            parse_argv.extend([
                "--lookup", "{}:{}".format(line, column)
            ])

        # If we are running Valgrind, we are not interested in actual testsuite
        # results. We just want to know if we have memory issues.
        if self.run_valgrind:
            has_errors = self.memory_errors.parse_from_valgrind_xml_output(
                testcase, get_valgrind_xml_report(parse_argv)
            )
            self.report_testcase(testcase, has_errors)
            return

        # If we are running a debugger, results aren't important, too.
        if self.debug:
            parse_argv = [self.debugger, '--args'] + parse_argv
            print(parse_argv)
            sp.check_call(parse_argv)
            return

        out = sp.check_output(parse_argv)

        with open(testcase.expected_file) as f:
            expected = f.read()

        has_failed = out != expected
        self.report_testcase(testcase, has_failed)

        if has_failed:
            self.console.write((Generic.Deleted,
                'EXPECTED: {}'.format(expected.strip())))
            self.console.endl()
            self.console.write((Generic.Inserted,
                'OUT:      {}'.format(out.strip())))
            self.console.endl()

            if self.rewrite:
                self.stats.add(Stats.REWRITTEN)
                self.console.write((Comment, 'Rewriting test'))
                self.console.endl()
                with open(testcase.expected_file, 'w') as f:
                    f.write(out)


#
# Output formatting utilities
#

def updated(dict1, dict2):
    """Return a copy of dict1 updated using dict2."""
    result = dict1.copy()
    result.update(dict2)
    return result


class OutputStyle(pygments.style.Style):
    default_style = ''
    styles = updated(pygments.styles.get_style_by_name('native').styles, {
        Generic.OK: '#6ab825',
        Generic.Crash: 'bold #ff0000',
        Generic.Rewritten: '#ed9d13',
    })


class ConsoleColorConfig:
    NONE = 0
    COLORS_16 = 1
    COLORS_256 = 2


class Console(object):

    def __init__(self, stream, color_config):
        self.stream = stream

        formatter_name = {
            ConsoleColorConfig.NONE:       'text',
            ConsoleColorConfig.COLORS_16:  'terminal',
            ConsoleColorConfig.COLORS_256: 'terminal256',
        }[color_config]
        self.formatter = pygments.formatters.get_formatter_by_name(
            formatter_name, style=OutputStyle
        )

    def flush(self):
        self.stream.flush()

    def write(self, *tokens):
        self.stream.write(pygments.format(tokens, self.formatter))

    def endl(self):
        self.stream.write('\n')


#
# Valgrind-based checking utilities
#

# We want our errors to be hashable, so use tuples instead of lists.
StackFrame = collections.namedtuple('StackFrame', 'ip obj fn dir file line')
Error = collections.namedtuple('Error', 'message stack')


class TempFile(object):
    """
    Guard used to create a temporary file that can be passed to subprograms.
    """
    def __enter__(self):
        fd, self.path = tempfile.mkstemp(prefix='libadalang-valgrind-')
        os.close(fd)
        return self.path

    def __exit__(self, value, type, traceback):
        os.remove(self.path)


def get_valgrind_xml_report(argv):
    """
    Run "argv" under valgrind, get the XML report for it and return it.
    """
    with TempFile() as temp_file:
        valgrind_args = [
            'valgrind',
            '--xml=yes', '--xml-file={}'.format(temp_file),
        ]
        with open(os.devnull, 'r') as devnull:
            sp.check_call(valgrind_args + argv, stdout=devnull)

        with open(temp_file) as xml_file:
            return etree.parse(xml_file).getroot()


def format_stack_frame(frame):
    """Format a stack frame for pretty-printing."""
    labels = [
        (Text, 'At '), (Number.Integer, frame.ip),
    ]
    if frame.fn:
        labels.extend([
            (Text, ', in '),
            (Name.Function, frame.fn),
        ])

    if frame.dir and frame.file and frame.line:
        labels.extend([
            (Text, ', from '),
            (String.Other, os.path.join(frame.dir, frame.file)),
            (Punctuation, ':'), (Number.Integer, frame.line),
        ])
    else:
        labels.extend([
            (Text, ', from '),
            (String.Other, frame.obj),
        ])

    return labels


def get_child(node, tag):
    """
    Return the first child in `node` that has the given tag, or None if there
    is none.
    """
    for child in node:
        if child.tag == tag:
            return child
    return None


def get_text_in_child(node, tag):
    """
    Return the text contained in the node returned by `get_child`, or none
    if there is none.
    """
    child = get_child(node, tag)
    return None if child is None else child.text


class MemoryErrors(object):
    """
    Memory issues database.  Used to aggregate error that are similar across
    testcases.
    """

    def __init__(self):
        self.errors = collections.defaultdict(set)

    def parse_from_valgrind_xml_output(self, testcase, xml_root):
        has_errors = False
        for elt in xml_root:
            if elt.tag == 'error':
                has_errors = True
                message = get_text_in_child(get_child(elt, 'xwhat'), 'text')
                stack = []
                for frame in get_child(elt, 'stack'):
                    assert frame.tag == 'frame'
                    stack.append(StackFrame(
                        get_text_in_child(frame, 'ip'),
                        get_text_in_child(frame, 'obj'),
                        get_text_in_child(frame, 'fn'),
                        get_text_in_child(frame, 'dir'),
                        get_text_in_child(frame, 'file'),
                        get_text_in_child(frame, 'line'),
                    ))
                self.add(testcase, Error(message, tuple(stack)))
        return has_errors

    def add(self, testcase, error):
        self.errors[error].add(testcase)

    def print_report(self, console):
        first = True
        for error, testcases in self.errors.iteritems():
            if not first:
                console.endl()
            first = False

            console.write((Text, 'Occurred in: '))
            first_testcase = True
            for testcase in sorted(testcases):
                if not first_testcase:
                    console.write((Text, ', '))
                first_testcase = False
                console.write((Name.Variable, testcase.name))
            console.endl()

            for frame in error.stack:
                console.write((Text, '  '), *format_stack_frame(frame))
                console.endl()
            console.write((Generic.Error, error.message))
            console.endl()


parser = argparse.ArgumentParser(description='Execute the libadalang testsuite')
parser.add_argument('-w', '--write', default=[], nargs=2,
                    help='Write a test with name and input string')
parser.add_argument('-r', '--rewrite', dest='rewrite', action='store_true')
parser.add_argument('--valgrind', dest='valgrind', action='store_true')
parser.add_argument('--debug', '-g', action='store_true',
                    help='Run a test under GDB')
parser.add_argument('--debugger', '-G', default='gdb',
                    help='Program to use as a debugger')
parser.set_defaults(rewrite=False)
parser.add_argument('test-patterns', nargs='*',
                    help='If provided, run only tests that start with the'
                         ' given patterns. For instance: "delay_" for all'
                         ' tests whose name start with "delay_".')
args = parser.parse_args()

# Move to the testsuite directory
dr_path = path.dirname(path.realpath(__file__))
os.chdir(dr_path)

console = Console(sys.stdout, ConsoleColorConfig.COLORS_256)
testsuite = Testsuite(console, args.valgrind,
                      args.debug, args.debugger,
                      args.rewrite)

# Add the testcases to the queue
for cdir, subdirs, files in os.walk("."):
    testcase = Testcase(cdir[2:])
    if (
        cdir == '.'
        or not match_testcase(getattr(args, 'test-patterns'), testcase.name)
    ):
        continue
    testsuite.add_test(testcase)

    # When debugging, we want to run exactly one test.
    if args.debug:
        break

# And then run it!
testsuite_aborted = False
try:
    testsuite.run()
except KeyboardInterrupt:
    testsuite_aborted = True

# Report time...
if testsuite_aborted:
    console.endl()
    console.write((Punctuation, '*************************'))
    console.endl()
    console.write((Punctuation, '*** Testsuite aborted ***'))
    console.endl()
    console.write((Punctuation, '*************************'))
    console.endl()

if args.valgrind:
    console.endl()
    console.write((Generic.Heading, 'Valgrind report'))
    console.endl()
    console.write((Generic.Heading, '==============='))
    console.endl()
    testsuite.memory_errors.print_report(console)


console.endl()
console.write((Generic.Heading, 'Testsuite report'))
console.endl()
console.write((Generic.Heading, '================'))
console.endl()

for status, count in testsuite.stats:
    if not count:
        continue
    console.write(
        testsuite.stats.label_padding(status),
        testsuite.stats.label(status),
        (Text, ' '),
        (Number.Integer, str(count))
    )
    console.endl()
