import os
import os.path

from testsuite_support.base_driver import catch_test_errors
from testsuite_support.python_driver import PythonDriver


class SymbolResolutionDriver(PythonDriver):

    @catch_test_errors
    def tear_up(self):
        super(PythonDriver, self).tear_up()

        self.py_file = os.path.join(self.support_dir,
                                    'symbol_resolution_driver.py')
        self.tear_up_helper()
        self.py_args = self.input_sources
