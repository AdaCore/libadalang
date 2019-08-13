from __future__ import absolute_import, division, print_function

import os

from testsuite_support.base_driver import (
    BaseDriver, SetupError, catch_test_errors
)
from testsuite_support.python_driver import PythonRunner


class InlinePlaygroundDriver(BaseDriver):

    @catch_test_errors
    def tear_up(self):
        super(InlinePlaygroundDriver, self).tear_up()
        self.py_runner = PythonRunner(self)
        self.py_runner.setup_environment()
        self.py_file = os.path.join(self.py_runner.support_dir,
                                    'inline_playground.py')

        self.charset = self.test_env.get('charset', None)
        self.project_file = self.test_env.get('project_file', None)
        if 'input_sources' not in self.test_env:
            raise SetupError('Missing "input_sources" key in test.yaml')
        self.input_sources = self.test_env['input_sources']

    @catch_test_errors
    def run(self):
        args = list(self.input_sources)
        if self.charset:
            args.insert(0, '--charset={}'.format(self.charset))
        if self.project_file:
            args.insert(0, '-P{}'.format(self.project_file))
        self.py_runner.run(self.py_file, args)
