import argparse
import collections
import subprocess as sp
import os
import os.path as path
import sys
import tempfile
import xml.etree.ElementTree as etree

import pygments
import pygments.formatters
from pygments.token import *


class C:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'

def printcol(msg, color):
    print "{0}{1}{2}".format(color, msg, C.ENDC)

def match_testcase(test_patterns, testcase):
    """
    Return whether `testcase` matches at least one pattern.  Always return true
    when there is no test pattern.
    """
    return (
        not test_patterns
        or any(testcase.startswith(pattern) for pattern in test_patterns)
    )

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
        sp.check_call(valgrind_args + argv)

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
        for elt in xml_root:
            if elt.tag == 'error':
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

    def add(self, testcase, error):
        self.errors[error].add(testcase)

    def print_report(self, file=sys.stderr, pygmentize=False):
        formatter = pygments.formatters.get_formatter_by_name(
            'terminal256' if pygmentize else 'text',
            style='native'
        )

        def ptoks(*tokens):
            sys.stderr.write(pygments.format(tokens, formatter))
        def pendl():
            sys.stderr.write('\n')

        first = True
        for error, testcases in self.errors.iteritems():
            if not first:
                pendl()
            first = False

            ptoks((Text, 'Occurred in: '))
            first_testcase = True
            for testcase in sorted(testcases):
                if not first_testcase:
                    ptoks((Text, ', '))
                first_testcase = False
                ptoks((Name.Variable, testcase))
            pendl()

            for frame in error.stack:
                ptoks((Text, '  '), *format_stack_frame(frame))
                pendl()
            ptoks((Generic.Error, error.message))
            pendl()


parser = argparse.ArgumentParser(description='Execute the libadalang testsuite')
parser.add_argument('-w', '--write', default=[], nargs=2,
                    help='Write a test with name and input string')
parser.add_argument('-r', '--rewrite', dest='rewrite', action='store_true')
parser.add_argument('--valgrind', dest='valgrind', action='store_true')
parser.set_defaults(rewrite=False)
parser.add_argument('test-patterns', nargs='*',
                    help='If provided, run only tests that start with the'
                         ' given patterns. For instance: "delay_" for all'
                         ' tests whose name start with "delay_".')
args = parser.parse_args()

dr_path = path.dirname(path.realpath(__file__))
os.chdir(dr_path)

if args.write:
    rule_name = args.write[0]
    test_input = args.write[1]
    try:
        i = 0
        while os.path.exists("./{0}_{1}".format(rule_name, i)):
            i += 1
        dirname = "{0}_{1}".format(rule_name, i)
        os.mkdir(dirname)

        out = sp.check_output(["../build/bin/parse", "-r", rule_name, "--input", test_input])

        printcol("Success", C.OKGREEN)
        print "Got out : {0}".format(out)

        with open(path.join(dirname, "input"), "w") as f:
            f.write(rule_name + "\n")
            f.write(test_input)

        with open(path.join(dirname, "expected"), "w") as f:
            f.write(out)

    except Exception, e:
        printcol("Failed writing test for rule {0}".format(rule_name), C.FAIL)
        print e
else:
    memory_errors = MemoryErrors()
    num_passed = 0
    num_failed = 0
    num_rewriten = 0
    for cdir, subdirs, files in os.walk("."):
        testcase = cdir[2:]
        if (cdir == "."
            or not match_testcase(getattr(args, 'test-patterns'), testcase)):
            continue
        try:
            with open(path.join(cdir, "input")) as f:
                rule_name = f.readline().strip()
                input_text = f.read().strip()

            argv = [
                "../build/bin/parse", "-r", rule_name, "--input", input_text
            ]

            if args.valgrind:
                memory_errors.parse_from_valgrind_xml_output(
                    testcase, get_valgrind_xml_report(argv)
                )
                print "{0}Test valgrind{1} - {2}".format(C.OKGREEN, C.ENDC, testcase)
                continue

            out = sp.check_output(argv)

            with open(path.join(cdir, "expected")) as f:
                expected = f.read()

            if out == expected:
                print "{0}Test passed{1} - {2}".format(C.OKGREEN, C.ENDC, testcase)
                num_passed += 1
            else:
                print "{0}Test failed{1} - {2}".format(C.FAIL, C.ENDC, testcase)
                print "OUT : \t\t", out.strip()
                print "EXPECTED : \t", expected.strip()
                num_failed += 1
                if args.rewrite:
                    print "Rewriting test {0}{1}{2}".format(C.HEADER, testcase, C.ENDC)
                    with open(path.join(cdir, "expected"), "w") as f:
                        f.write(out)

        except Exception, e:
            printcol("Error with test {0}".format(testcase), C.FAIL)
            num_failed += 1
            import traceback
            traceback.print_exc()
            print e

    if args.valgrind:
        memory_errors.print_report(sys.stdout, pygmentize=True)
    print "SUMMARY : {0}{1} passed{2}, {3}{4} failed{5} {6}".format(
        C.OKGREEN, num_passed, C.ENDC, C.FAIL, num_failed, C.ENDC,
        ", {0}{1} rewritten{2}".format(C.WARNING, num_rewriten, C.ENDC) if args.rewrite else ""
    )
