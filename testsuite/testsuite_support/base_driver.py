import os.path

import gnatpython.fileutils as fileutils
from gnatpython.testsuite.driver import TestDriver


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

    def read_file(self, filename):
        """Return the content of `filename`."""
        with open(filename, 'r') as f:
            return f.read()

    def analyze(self):
        diff = fileutils.diff(self.expected_file, self.output_file)
        if diff:
            self.result.actual_output += diff
            self.result.set_status('FAILED', 'output is not as expected')
        else:
            self.result.set_status('PASSED')
