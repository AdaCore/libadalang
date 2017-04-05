from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

import os
import os.path

from testsuite_support.base_driver import (
    BaseDriver, catch_test_errors, SetupError,
)


class PythonDriver(BaseDriver):
    TIMEOUT = 300

    #
    # Driver entry poins
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

        if 'input_sources' not in self.test_env:
            raise SetupError('Missing "input_sources" key in test.yaml')
        self.input_sources = self.test_env['input_sources']

        self.check_file(self.py_file)
        self.check_file_list('"input_sources"', self.input_sources)

        self.runner.setup_environment()

    @catch_test_errors
    def run(self):
        self.runner.run(self.py_file, [])


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

    def setup_environment(self):
        """
        Make the common Python modules available from the testcase script.
        """
        try:
            pythonpath = os.environ['PYTHONPATH']
        except KeyError:
            pythonpath = self.support_dir
        else:
            pythonpath = '{}{}{}'.format(
                self.support_dir, os.path.pathsep, pythonpath
            )
        os.environ['PYTHONPATH'] = pythonpath
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
