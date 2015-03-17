import os.path
import pipes
import subprocess

from gnatpython.ex import Run, STDOUT
from gnatpython import fileutils
from gnatpython.testsuite.driver import TestDriver

from testsuite_support.valgrind import Valgrind


class TestError(Exception):
    """
    Helper exception to work with catch_test_errors: see below.
    """
    pass


def catch_test_errors(func):
    """
    Helper decorator for driver entry points.

    This returns a wrapper around func that catches TestError exceptions and
    that turns them into PROBLEM test statuses. Using exceptions is convenient
    to stop any method from any point: this simplifies the control flow.
    """

    def wrapper(self, *args, **kwargs):
        try:
            return func(self, *args, **kwargs)
        except TestError as exc:
            self.result.set_status('PROBLEM', exc.message)
    return wrapper


class BaseDriver(TestDriver):
    """
    Base class to provide common test driver helpers.

    Ideally, these should end up in GNATpython, but this base class acts as a
    staging area: once it has been proven that some feature is useful, it may
    be easier to submit it upstream...
    """

    TIMEOUT = None

    def tear_up(self):
        super(BaseDriver, self).tear_up()
        self.create_test_workspace()
        self.valgrind = (Valgrind(self.working_dir)
                         if self.global_env['options'].valgrind else None)

    def read_file(self, filename):
        """Return the content of `filename`."""
        with open(filename, 'r') as f:
            return f.read()

    # Convenience path builders

    @property
    def root_dir(self):
        """
        Return the absolute path to the repository root directory.
        """
        return os.path.join(os.path.abspath(self.test_dir), '..', '..')

    @property
    def test_dir(self):
        return self.test_env['test_dir']

    @property
    def working_dir(self):
        return os.path.join(self.global_env['working_dir'],
                            self.test_env['test_name'])

    @property
    def output_file(self):
        return os.path.join(self.working_dir, 'actual.out')

    @property
    def expected_file(self):
        return os.path.join(self.working_dir, 'test.out')

    #
    # Tear up helpers
    #

    def check_file(self, filename):
        """
        Check file presence

        If the file does not exist test is aborted.
        """
        if not os.path.isfile(filename):
            raise TestError('Missing mandatory file: {}'.format(filename))

    def create_test_workspace(self):
        """
        Create a test workspace.

        This function copies the test sources into the working directory.
        """

        fileutils.sync_tree(self.test_dir, self.working_dir)

    #
    # Run helpers
    #

    def run_and_check(self, argv, for_debug=False):
        """
        Run a subprocess with `argv` and check it completes with status code 0

        If `for_debug` is True, then the program is run under Valgrind/GDB if
        asked to in the main testsuite driver. For GDB runs, the test is
        automatically assumed to have failed.

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
            subprocess.check_call(argv, cwd=self.working_dir)
            raise TestError('Test was running from a debugger: no result')
            return

        # Run valgrind if asked to
        if self.valgrind:
            argv = self.valgrind.wrap_argv(argv)

        p = Run(argv, cwd=self.working_dir,
                timeout=self.TIMEOUT,
                output=self.output_file,
                error=STDOUT)
        if p.status != 0:
            self.result.actual_output += (
                '{} returned status code {}'.format(program, p.status))
            self.result.actual_output += self.read_file(self.output_file)
            raise TestError(
                '{} returned status code {}'.format(program, p.status))

    #
    # Analysis helpers
    #

    def analyze(self):
        rewrite = self.global_env['options'].rewrite
        failures = []

        # Check for the test output itself.
        diff = fileutils.diff(self.expected_file, self.output_file)
        if diff:
            if rewrite:
                new_baseline = self.read_file(self.output_file)
                with open(self.original_expected_file, 'w') as f:
                    f.write(new_baseline)
            self.result.actual_output += diff
            failures.append('output is not as expected{}'.format(
                ' (baseline updated)' if rewrite else ''
            ))

        # Check memory issues if asked to.
        if self.valgrind:
            errors = self.valgrind.parse_report()
            if errors:
                self.result.actual_output += (
                    'Valgrind reported the following errors:\n{}'.format(
                        self.valgrind.format_report(errors)
                    )
                )
                failures.append('memory isuses detected')

        if failures:
            self.result.set_status('FAILED', ' | '.join(failures))
        else:
            self.result.set_status('PASSED')
