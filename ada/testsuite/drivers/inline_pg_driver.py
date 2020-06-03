import os

from e3.testsuite.driver.classic import TestAbortWithError

from drivers.base_driver import BaseDriver
from drivers.python_driver import PythonRunner


class InlinePlaygroundDriver(BaseDriver):

    def run(self):
        py_runner = PythonRunner(self)
        py_file = os.path.join(py_runner.support_dir, 'inline_playground.py')

        charset = self.test_env.get('charset', None)
        project_file = self.test_env.get('project_file', None)
        if 'input_sources' not in self.test_env:
            raise TestAbortWithError(
                'Missing "input_sources" key in test.yaml'
            )
        input_sources = self.test_env['input_sources']

        args = list(input_sources)
        if charset:
            args.insert(0, '--charset={}'.format(charset))
        if project_file:
            args.insert(0, '-P{}'.format(project_file))
        py_runner.run(py_file, args)
