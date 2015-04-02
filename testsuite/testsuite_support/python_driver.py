import os.path
import pipes

from gnatpython import fileutils
from gnatpython.ex import Run, STDOUT

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

        if 'input_sources' not in self.test_env:
            raise SetupError('Missing "input_sources" key in test.yaml')
        input_sources = self.test_env['input_sources']

        self.check_file(os.path.join(self.test_dir, 'test.py'))
        self.check_file_list(input_sources)
        self.check_file(self.expected_file)

    @catch_test_errors
    def run(self):
        self.run_and_check(['python', 'test.py'], for_debug=True)
