from __future__ import absolute_import, division, print_function

import difflib
import os
import os.path

from testsuite_support.base_driver import (BaseDriver, SetupError,
                                           catch_test_errors)
from testsuite_support.python_driver import PythonRunner


class NameResolutionDriver(BaseDriver):

    @property
    def run_python(self):
        # TODO: right now, the Python driver does not support project handling
        return (self.py_runner.is_python_api_available
                and not self.with_default_project)

    @property
    def run_python_only(self):
        return self.test_env.get('run_python_only', False)

    @catch_test_errors
    def tear_up(self):
        super(NameResolutionDriver, self).tear_up()

        self.py_runner = PythonRunner(self)
        self.py_file = os.path.join(self.py_runner.support_dir,
                                    'nameres.py')

        if 'input_sources' not in self.test_env:
            raise SetupError('Missing "input_sources" key in test.yaml')
        self.input_sources = self.test_env['input_sources']

        self.charset = self.test_env.get('charset', None)
        self.with_default_project = self.test_env.get('with_default_project',
                                                      False)

        if self.run_python:
            self.py_runner.setup_environment()

    @catch_test_errors
    def run(self):
        args = list(self.input_sources)
        if self.charset:
            args.insert(0, '--charset={}'.format(self.charset))
        if self.with_default_project:
            args.insert(0, '--with-default-project')

        # Depending on whether Python is available and whether we want to run
        # only the Python driver, run both the Python and the Ada drivers for
        # name resolution.
        if self.run_python:
            py_output = self.py_runner.run(self.py_file, args)
            if self.run_python_only:
                return

        ada_output = self.run_and_check(
            ['nameres'] + args,
            for_debug=True, memcheck=True, append_output=not self.run_python
        )
        if not self.run_python:
            return

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
