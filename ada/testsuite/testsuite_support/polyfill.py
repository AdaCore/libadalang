"""
Provide in-place substitutes for our GNATpython-based testsuite.

This modules provides various items that are compatible with GNATpython in
order to make it possible to run our GNATpython-based testsuite without
GNATpython itself (hence the "polyfill" name).
"""

import argparse
import collections
import difflib
from functools import partial
from itertools import imap
import os.path
import shutil
import subprocess
import sys
import yaml

from parallel_map import pmap


class _Main(object):

    """Dummy class for the BaseTestsuite.main attribute."""

    def __init__(self, add_option):
        self.add_option = add_option


class Colors(object):

    """Console colors handling.

    This class simplifies the integration of console escape sequences in output
    strings. It also make it possible to transparently disable colored outputs.
    """

    def _seq(chars):
        return '\x1b[{}m'.format(chars)

    basic_colors = {
        'reset':   _seq('0'),
        'bold':    _seq('1'),
        'red':     _seq('31'),
        'green':   _seq('32'),
        'yellow':  _seq('33'),
        'blue':    _seq('34'),
        'magenta': _seq('35'),
        'cyan':    _seq('36'),
        'gray':    _seq('37'),
    }

    def __init__(self):
        self.disable()

    def enable(self):
        """Enable colored output."""
        for color, seq in self.basic_colors.iteritems():
            setattr(self, color, seq)

    def disable(self):
        """Disable colored output."""
        for color, seq in self.basic_colors.iteritems():
            setattr(self, color, '')


class ReportWriter(object):

    """Write report to files and standard output."""

    status_to_color = {
        'OK':      'green',
        'PASSED':  'green',
        'FAIL':    'red',
        'PROBLEM': 'red',
        'XFAIL':   'cyan',
        'UOK':     'yellow',
    }

    def __init__(self, report_dir, colors, show_error_output):
        """Create a ReportWriter.

        :param str report_dir: Path to the directory in which the report is to
            be written.
        :param Colors colors: Control for colored messaged in the standard
            output.
        :param bool show_error_output: If true, display testcases error output
            when they are failing.
        """
        self.report_dir = report_dir
        self.colors = colors
        self.show_error_output = show_error_output
        self.summary_file = open(os.path.join(self.report_dir, 'results'), 'w')
        self.hits = collections.defaultdict(int)

    def get_color(self, status):
        """Return the color escape sequence corresponding to a status.

        :type status: str
        :rtype: str
        """
        color_name = self.status_to_color.get(status, 'red')
        return getattr(self.colors, color_name)

    def write(self, name, status, message, data, output):
        """Add the result of a testcase to the report.

        :param str name: Name for the testcase.
        :param str status: Status for the testcase.
        :param str|None message: In case of non-success, message for the
            testcase.
        :param dict|None data: "test_data" record associated to this testcase,
            if any.
        :param str|None output: Output for this testcase, if any.
        """
        self.hits[status] += 1

        # Output a single line to the standard output
        fmt_string = ('{clr_st}{st: <8}{clr_reset} {name}: {msg}'
                      if message else
                      '{clr_st}{st: <8}{clr_reset} {name}')
        print fmt_string.format(
            st=status,
            name=name,
            msg=message,
            clr_reset=self.colors.reset,
            clr_st=self.get_color(status)
        )
        # ... or more if told to do so.
        if (self.show_error_output and
                output.strip() and
                status not in ('PASSED', 'OK', 'UOK', 'XFAIL')):
            print '{clr.red}{output}{clr.reset}'.format(
                clr=self.colors,
                output=''.join('  {}\n'.format(line.rstrip())
                               for line in output.rstrip().split('\n')),
            )

        # Likewise in the report summary
        self.summary_file.write('{}:{}:{}\n'.format(
            name, status, message or ''
        ))

        # And finally output the full report record for this testcase
        test_yaml = {
            'actual_output': output,
            'msg': message,
            'status': status,
            'test_env': data,
        }
        with open(os.path.join(self.report_dir,
                               '{}.yaml'.format(name)), 'w') as f:
            yaml.dump(test_yaml, f)

    def print_hits(self):
        """Display statistics for statuses on the standard output."""
        print 'Summary:'
        for status in sorted(self.hits):
            print '  {}{}{}: {}'.format(
                self.get_color(status),
                status,
                self.colors.reset,
                self.hits[status]
            )

    def close(self):
        """Close the files open for this report."""
        self.summary_file.close()


class BaseTestsuite(object):

    """Base class for testsuites.

    In order to create a testsuite:

      * subclass it;
      * override the TEST_SUBDIR and DRIVERS class attributes;
      * override its hooks depending on your needs;
      * instantiate it and run the testsuite_main method.
    """

    TEST_SUBDIR = None
    """
    Name of the subdirectory that contains testcases. Must be overridden in
    subclasses.
    """

    DRIVERS = None
    """
    Dictionnary that associates driver names to driver classes (i.e. TestDriver
    subclasses).
    """

    def __init__(self, root_dir):
        """Create a testsuite instance.

        :param str root_dir: Path to the directory that hosts the testsuite.
        """
        self.colors = Colors()
        self.report_writer = None

        # Determine important paths
        self.root_dir = os.path.abspath(root_dir)
        self.test_dir = os.path.join(self.root_dir, self.TEST_SUBDIR)
        self.working_dir = os.path.join(self.root_dir, 'tmp')
        if not os.path.isdir(self.working_dir):
            os.mkdir(self.working_dir)

        # This will be available to both subclasses and TestDriver instances
        self.global_env = {
            'working_dir': self.working_dir,
            'options': None,  # testsuite_main will put parsed arguments there
        }

        # Create an argument parser, define built-in options and then let
        # subclasses extend it.
        self.arg_parser = argparse.ArgumentParser(
            description='Run the testsuite.'
        )
        self.main = _Main(self.arg_parser.add_argument)
        self.arg_parser.add_argument(
            '--enable-color', action='store_true',
            default=os.isatty(sys.stdout.fileno()),
            help='Enable colored console output'
        )
        self.arg_parser.add_argument(
            '--show-error-output', action='store_true',
            help='Show diff for test failure'
        )

        # Try to get a sane default for the cpu count
        try:
            import multiprocessing
            cpu_count = multiprocessing.cpu_count()
        except Exception:
            cpu_count = 1

        self.arg_parser.add_argument(
            '--jobs', '-j', type=int, default=cpu_count,
            help='Number of parallel jobs to be used for testing'
        )

        self.arg_parser.add_argument(
            'testcases', nargs='*',
            help='The list of testcases to run. Run everything is none is'
                 ' given.'
        )
        self.add_options()

    def testsuite_main(self, args=None):
        """Run the testsuite.

        :param [str]|None args: If passed, used as the command line argument
        list. Otherwise, use sys.argv instead.
        """
        self.args = self.arg_parser.parse_args(args)

        self.global_env['options'] = self.args
        self.report_writer = ReportWriter(
            self.working_dir, self.colors, self.args.show_error_output
        )

        if self.args.enable_color:
            self.colors.enable()
        else:
            self.colors.disable()

        self.tear_up()

        # We'll use regular map if there's only one job
        map_fn = (imap if self.args.jobs == 1
                  else partial(pmap, nb_threads=self.args.jobs))

        try:
            for r in map_fn(self._run_testcase, self._iter_testcases()):
                self.report_writer.write(*r)
        except KeyboardInterrupt:
            sys.stderr.write('{}Aborting after keyboard interrupt{}\n'.format(
                self.colors.bold + self.colors.red,
                self.colors.reset
            ))
            # The following statements are not supposed to be blocking, so it's
            # fine to execute them even after a keyboard interrupt.
        self.tear_down()
        self.report_writer.print_hits()
        self.report_writer.close()

    def _run_testcase(self, test_dir):
        """Helper for testsuite_main: run the testcase in test_dir."""
        status = None
        message = None

        test_dir = os.path.abspath(test_dir)
        test_relpath = os.path.relpath(test_dir, self.test_dir)
        test_name = test_relpath.replace(os.path.sep, '-')
        test_data = {}

        # First load data provided by this testcase
        try:
            with open(os.path.join(test_dir, 'test.yaml'), 'r') as f:
                test_data = yaml.load(f)
        except (IOError, yaml.error.YAMLError) as exc:
            status = 'PROBLEM'
            message = 'Invalid test.yaml: {}'.format(exc)

        # Then instantiate the driver to run it
        if not status:
            try:
                driver_name = test_data['driver']
            except KeyError:
                status = 'PROBLEM'
                message = '"driver" missing in test.yaml'
        if not status:
            try:
                driver_class = self.DRIVERS[driver_name]
            except KeyError:
                status = 'PROBLEM'
                message = 'Unknown driver: {}'.format(driver_name)

        # Finally run the testcase and write a report for it
        if not status:
            test_data.update({
                'test_dir': test_dir,
                'test_relpath': test_relpath,
                'test_name': test_name,
            })
            testcase = driver_class(test_data, self.global_env)
            testcase.tear_up()
            if not testcase.has_status:
                testcase.run()
                testcase.tear_down()
            if not testcase.has_status:
                testcase.analyze()

            status = testcase.result.status
            message = testcase.result.message
            output = testcase.result.actual_output
        else:
            output = None

        return test_name, status, message, test_data, output

    def _iter_testcases(self):
        """Yield subdirectory paths for testcases."""
        for root_dir in self.args.testcases or [self.test_dir]:
            for root, dirs, files in os.walk(root_dir):
                if 'test.yaml' in files:
                    yield root

    # User hooks

    def add_options(self):
        """Add arguments to the argument parser.

        This method is called in the constructor when it is time to add
        arguments to the parser. Subclasses are expected to override this
        method in order to do it.
        """
        pass

    def tear_up(self):
        """Prepare the environment to run the testsuite.

        This method is called right before looking for testcases and running
        them. Subclasses are expected to override this method in order to
        execute specific actions in this context.
        """
        pass

    def tear_down(self):
        """Cleanup the environment after a testsuite run.

        This method is called right after the last testcase is run.  Subclasses
        are expected to override this method in order to execute specific
        actions in this context.
        """
        pass


class _Result(object):

    """Hold the status for a testcase run."""

    def __init__(self):
        self.status = None
        self.message = None
        self.actual_output = ''

    def set_status(self, status, message=None):
        """Set the status for this testcase.

        :param str status: Status for this testcase run.
        :param str|None message: If the test does not pass, a message is
            expected to tell how it failed.
        """
        self.status = status
        self.message = message

    @property
    def has_status(self):
        """Return whether the set_status method was called.

        :rtype: bool
        """
        return self.status is not None


class TestDriver(object):

    """Base class for test drivers.

    Testsuites must define at least one test driver that specifies how to run
    testcases and how to analyze results. Instances represent specific
    testcase runs.
    """

    def __init__(self, test_data, global_env):
        """
        Create a test driver for test_data.

        :param dict test_data: A YAML object coming from a "test.yaml" file.
        :param dict global_env: The testsuite global environment.
        """
        self.global_env = global_env
        self.test_env = test_data
        self.result = _Result()

    @property
    def has_status(self):
        """Return whether the status was set for this testcase.

        :rtype: bool
        """
        return self.result.has_status

    # User hooks

    def tear_up(self):
        """Prepare the environment to run the testcase.

        This method is called right before running the testcase.  Subclasses
        are expected to override this method in order to execute specific
        actions in this context.
        """

    def tear_down(self):
        """Cleanup the environment after a testcase run.

        This method is called right after running the testcase.  Subclasses
        are expected to override this method in order to execute specific
        actions in this context.
        """
        pass

    def run(self):
        """Run the testcase.

        Subclasses are expected to override this method in order to specify how
        to run a testcase.
        """
        pass

    def analyze(self):
        """Compute the status for this testcase.

        This method is called after the testsuite run when none of tear_up and
        "run" set the status for this testcase.
        """
        pass


class fileutils(object):

    """Namespace for file manipulation helpers."""

    @staticmethod
    def sync_tree(source, destination):
        """Copy the "source" directory into the "destination" one."""
        # gnatpython.fileutils.sync_tree actually does not remove the
        # destination before copying.  However, this function is used only to
        # setup testcase workspaces, which we will not cleanup afterwards.
        if os.path.exists(destination):
            shutil.rmtree(destination)
        shutil.copytree(source, destination)

    @staticmethod
    def diff(a, b, ignore_white_chars=None):
        """Return a string representing the diff between files "a" and "b"."""
        with open(a, 'r') as f:
            a_content = f.read().split('\n')
        with open(b, 'r') as f:
            b_content = f.read().split('\n')
        return '\n'.join(difflib.unified_diff(a_content, b_content,
                                              fromfile=a, tofile=b,
                                              lineterm=''))


class Run(object):

    """Run a subprocess."""

    def __init__(self, argv, cwd=None, timeout=None, output=None, error=None):
        # "timeout" is not implemented, error is ignored
        p = subprocess.Popen(
            argv, cwd=cwd,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT
        )
        stdout, _ = p.communicate()
        with open(output, 'w') as f:
            f.write(stdout)
        self.status = p.returncode


STDOUT = None
