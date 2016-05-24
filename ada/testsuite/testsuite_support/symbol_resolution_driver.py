import difflib
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
        self.run_python_only = self.test_env.get('run_python_only', False)

    @catch_test_errors
    def run(self):
        # Run both the Python and the Ada drivers for symbol resolution
        py_output = self.run_and_check(
            [self.python_interpreter, self.py_file] + self.py_args,
            for_debug=True
        )
        if self.run_python_only:
            return

        ada_output = self.run_and_check(
            ['symres'] + self.input_sources,
            for_debug=True, memcheck=True, append_output=False,
        )

        diff = '\n'.join(difflib.unified_diff(py_output.splitlines(),
                                              ada_output.splitlines(),
                                              fromfile='python',
                                              tofile='ada',
                                              lineterm=''))
        if diff:
            with open(self.output_file, 'a') as f:
                f.write('\n')
                f.write('The Python driver and the Ada driver did not return'
                        ' the same result:\n')
                f.write(diff)
