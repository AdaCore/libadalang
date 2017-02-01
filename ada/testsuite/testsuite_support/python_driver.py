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

    def __init__(self, *args, **kwargs):
        super(PythonDriver, self).__init__(*args, **kwargs)
        self.py_file = None
        self.py_args = []

    def tear_up_helper(self):
        if self.disable_shared:
            self.result.set_status(
                'DEAD',
                'Cannot test the Python API without shared libraries'
            )
        if self.disable_python:
            self.result.set_status('DEAD', 'Python API testing disabled')

        if 'input_sources' not in self.test_env:
            raise SetupError('Missing "input_sources" key in test.yaml')
        self.input_sources = self.test_env['input_sources']

        if not self.py_file:
            self.py_file = 'test.py'
            self.check_file(self.py_file)

        self.check_file_list('"input_sources"', self.input_sources)

        # Make the common Python modules available from the testcase script
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
        os.environ['LIBADALANG_DISABLE_SHARED'] = str(int(self.disable_shared))

    @catch_test_errors
    def tear_up(self):
        super(PythonDriver, self).tear_up()
        self.tear_up_helper()

    @catch_test_errors
    def run(self):
        self.run_and_check(
            [self.python_interpreter, self.py_file] + self.py_args,
            for_debug=True
        )

    @property
    def support_dir(self):
        """
        Return the absolute path to the directory for support Python modules.
        """
        return os.path.join(self.testsuite_dir, 'python_support')
