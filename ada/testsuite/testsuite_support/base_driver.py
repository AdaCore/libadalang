from __future__ import absolute_import, division, print_function

import os
import os.path
import pipes
import subprocess
import sys

# pyflakes off
with_gnatpython = False
if not os.environ.get('WITHOUT_GNATPYTHON'):
    try:
        from gnatpython import fileutils
        from gnatpython.ex import PIPE, Run, STDOUT
        from gnatpython.testsuite.driver import TestDriver
    except ImportError:
        pass
    else:
        with_gnatpython = True
if not with_gnatpython:
    from testsuite_support.polyfill import (PIPE, Run, STDOUT, TestDriver,
                                            fileutils)
# pyflakes on

import testsuite_support.discriminants as discriminants
from testsuite_support.valgrind import Valgrind


class SetupError(Exception):
    """
    Exception to raise when the testcase is invalid.

    Helper exception to work with catch_test_errors: see below.
    """
    pass


class TestError(Exception):
    """
    Exception to raise when the testcase fails.

    Helper exception to work with catch_test_errors: see below.
    """
    pass


def catch_test_errors(func):
    """
    Helper decorator for driver entry points.

    This returns a wrapper around func that catches SetupError and TestError
    exceptions and that turns them into the appropriate test status. Using
    exceptions is convenient to stop any method from any point: this simplifies
    the control flow.
    """

    def wrapper(self, *args, **kwargs):
        try:
            return func(self, *args, **kwargs)
        except SetupError as exc:
            self.set_setup_error(exc.message)
        except TestError as exc:
            self.set_failure(exc.message)
    return wrapper


class BaseDriver(TestDriver):
    """
    Base class to provide common test driver helpers.

    Ideally, these should end up in GNATpython, but this base class acts as a
    staging area: once it has been proven that some feature is useful, it may
    be easier to submit it upstream...
    """

    DEFAULT_TIMEOUT = 300

    def tear_up(self):
        super(BaseDriver, self).tear_up()
        self.create_test_workspace()

        # If requested, skip internal testcases
        if (
            self.global_env['options'].skip_internal_tests and
            self.test_env['test_name'].startswith('internal-tests')
        ):
            self.result.set_status('DEAD', 'Skipping internal testcase')
            return

        discriminants.add_discriminants(self.global_env['options']
                                        .discriminants)

        # If asked to run under Valgrind, prepare a Valgrind instance
        if self.global_env['options'].valgrind:
            valgrind_supp = self.test_env.get('valgrind_suppressions', None)
            if valgrind_supp:
                valgrind_supp = os.path.join(self.test_dir, valgrind_supp)

            self.valgrind = Valgrind(self.testsuite_dir, self.working_dir(),
                                     valgrind_supp)
        else:
            self.valgrind = None
        self.valgrind_errors = []

        self.check_file(self.expected_file)

        # Load the expected failure matcher for this testcase
        try:
            expect_failure_matcher = discriminants.Matcher.from_json(
                self.test_env.get('expect_failure', [])
            )
        except ValueError as exc:
            raise SetupError('Invalid "expect_failure" entry: {}'.format(exc))

        # Determine whether we do have an expected failure
        self.expect_failure_comment = expect_failure_matcher.matches()
        if self.expect_failure_comment is None:
            self.expect_failure = False
        else:
            # Because of wrapping in the YAML file, we can get multi-line
            # strings, which is not valid for comments.
            self.expect_failure_comment = (self.expect_failure_comment
                                           .replace('\n', ' ').strip())
            self.expect_failure = True

        # Use the specified timeout if any, otherwise fallback to the default
        # one.
        try:
            timeout = self.test_env['timeout']
        except KeyError:
            self.timeout = self.DEFAULT_TIMEOUT
        else:
            if (not isinstance(timeout, int)
                    or timeout < 0):
                raise SetupError('Invalid "timeout" entry: expected a positive'
                                 ' number of seconds, got {} instead'.format(
                                     timeout))
            self.timeout = timeout

    def read_file(self, filename):
        """Return the content of `filename`."""
        with open(filename, 'r') as f:
            return f.read()

    def set_setup_error(self, message):
        self.result.set_status('PROBLEM', message)

    def set_failure(self, message):
        if self.expect_failure:
            self.result.set_status('XFAIL', '{}{}'.format(
                message,
                ' ({})'.format(self.expect_failure_comment)
                if self.expect_failure_comment else ''
            ))
        else:
            self.result.set_status('FAILED', message)

    def set_passed(self):
        if self.expect_failure:
            msg = (
                'Failure was expected: {}'.format(self.expect_failure_comment)
                if self.expect_failure_comment else None
            )
            self.result.set_status('UOK', msg)
        else:
            self.result.set_status('PASSED')

    # Convenience path builders

    @property
    def testsuite_dir(self):
        """Return the absolute path to the testsuite root directory."""
        result = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                              '..')
        return os.path.abspath(result)

    @property
    def test_dir(self):
        """Return the path of the current testcase directory."""
        return self.test_env['test_dir']

    def working_dir(self, *args):
        """
        Return the working dir, plus any path elements joined to it if passed
        in *args.
        """
        return os.path.join(self.global_env['working_dir'],
                            self.test_env['test_name'], *args)

    @property
    def output_file(self):
        return self.working_dir('actual.out')

    @property
    def expected_file(self):
        return self.working_dir('test.out')

    @property
    def original_expected_file(self):
        return os.path.join(self.test_dir, 'test.out')

    #
    # Tear up helpers
    #

    @property
    def disable_shared(self):
        return self.global_env['options'].disable_shared

    @property
    def disable_python(self):
        return self.global_env['options'].disable_python

    @property
    def python_interpreter(self):
        choices = {
            'default': self.global_env['options'].with_python or 'python',
            'self':    sys.executable,
        }

        key = self.test_env.get('python_interpreter', 'default')
        if not isinstance(key, basestring):
            raise SetupError(
                'Invalid "python_interpreter" key in test.yaml: it must'
                ' contain a string, got a {} instead'.format(
                    type(key).__name__))
        try:
            return choices[key]
        except KeyError:
            raise SetupError(
                'Invalid "python_interpreter" key in test.yaml: got {}, while'
                ' expecting one of: {}'.format(
                    repr(key), ', '.join(repr(k) for k in choices)))

    def check_file(self, filename):
        """
        Check file presence.

        If the file does not exist test is aborted.
        """
        if not os.path.isfile(os.path.join(self.test_dir, filename)):
            raise SetupError('Missing mandatory file: {}'.format(filename))

    def check_file_list(self, what, file_list, can_be_empty=True):
        """Raise a SetupError if `file_list` is not a list of existing files.

        Also raise an error if it is an empty list while `can_be_empty` is
        False.
        """
        # First check we have a list of strings
        if (not isinstance(file_list, list) or
                (not can_be_empty and len(file_list) == 0) or
                not all(isinstance(fn, basestring) for fn in file_list)):
            empty_msg = 'non-empty '
            raise SetupError(
                '{} must be a {}list of strings'.format(what, empty_msg))

        # Then check that these are existing files
        for filename in file_list:
            self.check_file(filename)

    def create_test_workspace(self):
        """
        Create a test workspace.

        This function copies the test sources into the working directory.
        """

        fileutils.sync_tree(self.test_dir, self.working_dir())

    @staticmethod
    def diff(first_file, second_file):
        return fileutils.diff(first_file, second_file,
                              ignore_white_chars=False)

    #
    # Run helpers
    #

    def run_and_check(self, argv, for_debug=False, memcheck=False,
                      append_output=True):
        """
        Run a subprocess with `argv` and check it completes with status code 0.

        If `for_debug` is True, then the program is run under GDB if asked to
        in the main testsuite driver. For GDB runs, the test is automatically
        assumed to have failed.

        If `memcheck` is True then the program is run under Valgrind if asked
        to in the main testsuite driver. Any memory issue will be reported and
        turned into a testcase failure.

        In case of failure, the test output is appended to the actual output
        and a TestError is raised.
        """
        opts = self.global_env['options']
        program = argv[0]

        # If we are running a debugger, we aren't even interested in the
        # result.
        if for_debug and opts.debug:
            print('Running {} ({}) under a debugger...'.format(
                program,
                self.test_env['test_name']
            ))
            argv = [opts.debugger, '--args'] + argv
            print(' '.join(pipes.quote(arg) for arg in argv))
            subprocess.check_call(argv, cwd=self.working_dir())
            raise TestError('Test was running from a debugger: no result')
            return

        # Run valgrind if asked to
        if memcheck and self.valgrind:
            argv = self.valgrind.wrap_argv(argv)

        p = Run(argv, cwd=self.working_dir(),
                timeout=self.timeout,
                output=PIPE,
                error=STDOUT)

        if append_output:
            with open(self.output_file, 'a') as f:
                f.write(p.out)

        if p.status != 0:
            self.result.actual_output += (
                '{} returned status code {}\n'.format(program, p.status))
            self.result.actual_output += p.out
            raise TestError(
                '{} returned status code {}'.format(program, p.status))

        if memcheck and self.valgrind:
            self.valgrind_errors.extend(self.valgrind.parse_report())

        return p.out

    @property
    def gpr_scenario_vars(self):
        """
        Return the project scenario variables to pass to GPRbuild.

        :rtype: list[str]
        """
        library_type = 'static' if self.disable_shared else 'relocatable'
        return ['-XLIBRARY_TYPE={}'.format(library_type),
                '-XXMLADA_BUILD={}'.format(library_type)]

    #
    # Analysis helpers
    #

    def analyze(self):
        rewrite = (self.global_env['options'].rewrite
                   and not self.expect_failure)
        failures = []

        # Check for the test output itself
        diff = self.diff(self.expected_file, self.output_file)
        if diff:
            if rewrite:
                new_baseline = self.read_file(self.output_file)
                with open(self.original_expected_file, 'w') as f:
                    f.write(new_baseline)
            self.result.actual_output += diff
            failures.append('output is not as expected{}'.format(
                ' (baseline updated)' if rewrite else ''
            ))

        # Check memory issues if asked to
        if self.valgrind_errors:
            self.result.actual_output += (
                'Valgrind reported the following errors:\n{}'.format(
                    self.valgrind.format_report(self.valgrind_errors)
                )
            )
            failures.append('memory issues detected')

        if failures:
            self.set_failure(' | '.join(failures))
        else:
            self.set_passed()
