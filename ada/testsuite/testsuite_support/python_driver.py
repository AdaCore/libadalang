from testsuite_support.base_driver import (
    BaseDriver, catch_test_errors, SetupError,
)


class PythonDriver(BaseDriver):
    TIMEOUT = 300

    #
    # Driver entry poins
    #

    @catch_test_errors
    def tear_up(self):
        super(PythonDriver, self).tear_up()

        if self.disable_shared:
            self.result.set_status(
                'DEAD',
                'Cannot test the Python API without shared libraries'
            )
        if self.disable_python:
            self.result.set_status('DEAD', 'Python API testing disabled')

        if 'input_sources' not in self.test_env:
            raise SetupError('Missing "input_sources" key in test.yaml')
        input_sources = self.test_env['input_sources']

        self.check_file('test.py')
        self.check_file_list('"input_sources"', input_sources)

    @catch_test_errors
    def run(self):
        self.run_and_check([self.python_interpreter, 'test.py'],
                           for_debug=True)
