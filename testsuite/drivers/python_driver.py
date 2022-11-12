import os
import os.path
import sys

from e3.testsuite.driver.classic import TestSkip

from drivers.base_driver import BaseDriver


class PythonDriver(BaseDriver):

    py_file = 'test.py'

    def run(self):
        runner = PythonRunner(self)

        if not runner.is_python_api_available:
            raise TestSkip('Cannot test the Python API without shared'
                           ' libraries')
        if self.disable_python:
            raise TestSkip('Python API testing disabled')

        input_sources = self.test_env.get('input_sources', [])
        project_file = self.test_env.get('project_file', None)
        self.check_file(self.py_file)
        self.check_file_list('"input_sources"', input_sources)

        args = list(input_sources)

        # If there is a project file specified, add it to the arguments
        if project_file:
            self.check_file(project_file)
            args.append(f"-P{project_file}")

        runner.run(self.py_file, args)


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

    def run(self, py_file, py_args):
        """
        Run the given Python scripts with given arguments.
        """
        return self.driver.run_and_check(
            [self.interpreter, py_file] + py_args,
            env={
                'PYTHONPATH': self.add_paths(
                    os.environ.get('PYTHONPATH'),
                    self.support_dir,
                    self.internal_support_dir
                ),
                'LIBADALANG_ROOTDIR': os.path.join(
                    os.path.dirname(os.path.abspath(__file__)),
                    '..', '..',
                ),
                'LIBADALANG_DISABLE_SHARED': str(
                    int(self.driver.disable_shared)
                )
            }
        )

    @property
    def interpreter(self):
        """
        Return the Python interpreter to use.
        """
        return (sys.executable
                if self.driver.use_testsuite_python else
                self.driver.python_interpreter)

    @property
    def support_dir(self):
        """
        Return the absolute path to the directory for support Python modules.
        """
        return os.path.join(self.driver.env.root_dir, 'python_support')

    @property
    def internal_support_dir(self):
        """
        Return the absolute path to the directory for support Python modules
        for the internal testsuite.
        """
        return os.path.join(self.driver.env.root_dir, 'tests', 'internal',
                            'python_support')
