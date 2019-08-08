from __future__ import absolute_import, division, print_function

import os
import os.path

from testsuite_support.base_driver import BaseDriver, catch_test_errors


class PythonDriver(BaseDriver):

    #
    # Driver entry point
    #

    py_file = 'test.py'

    @catch_test_errors
    def tear_up(self):
        super(PythonDriver, self).tear_up()

        self.runner = PythonRunner(self)

        if not self.runner.is_python_api_available:
            self.result.set_status(
                'DEAD',
                'Cannot test the Python API without shared libraries'
            )
        if self.disable_python:
            self.result.set_status('DEAD', 'Python API testing disabled')

        self.input_sources = self.test_env.get('input_sources', [])

        self.check_file(self.py_file)
        self.check_file_list('"input_sources"', self.input_sources)

        self.runner.setup_environment()

    @catch_test_errors
    def run(self):
        self.runner.run(self.py_file, self.input_sources)


class PythonRunner(object):
    """
    Helper to factorize facilities to run Python scripts using Libadalang.
    """

    def __init__(self, driver):
        self.driver = driver

    @property
    def is_python_api_available(self):
        """
        Return if Libadalang's Python API is available.
        """
        return not self.driver.disable_shared

    @staticmethod
    def add_paths(old_path, *dirnames):
        """
        Helper to build environment paths: prepend items in `dirnames` to
        `old_path`. This inserts `os.path.pathsep` where appropriate.

        :type old_path: str
        :type dirnames: list[str]
        :rtype: str
        """
        new_paths = os.path.pathsep.join(dirnames)
        return ('{}{}{}'.format(new_paths, os.path.pathsep, old_path)
                if old_path else new_paths)

    def setup_environment(self):
        """
        Make the common Python modules available from the testcase script.
        """
        os.environ['PYTHONPATH'] = self.add_paths(
            os.environ.get('PYTHONPATH'),
            self.support_dir,
            self.internal_support_dir)
        os.environ['LIBADALANG_ROOTDIR'] = os.path.join(
            os.path.dirname(os.path.abspath(__file__)),
            '..', '..', '..'
        )
        os.environ['LIBADALANG_DISABLE_SHARED'] = str(
            int(self.driver.disable_shared)
        )

    def run(self, py_file, py_args):
        """
        Run the given Python scripts with given arguments.

        Make sure you called the "setup_environment" method once first.
        """
        return self.driver.run_and_check(
            [self.driver.python_interpreter, py_file] + py_args,
            for_debug=True
        )

    @property
    def support_dir(self):
        """
        Return the absolute path to the directory for support Python modules.
        """
        return os.path.join(self.driver.testsuite_dir, 'python_support')

    @property
    def internal_support_dir(self):
        """
        Return the absolute path to the directory for support Python modules
        for the internal testsuite.
        """
        return os.path.join(self.driver.testsuite_dir, 'tests', 'internal',
                            'python_support')
