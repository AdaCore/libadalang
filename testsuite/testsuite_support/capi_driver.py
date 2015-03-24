import os.path
import pipes

from gnatpython import fileutils
from gnatpython.ex import Run, STDOUT

from testsuite_support.base_driver import (
    BaseDriver, catch_test_errors, TestError,
)
from testsuite_support.valgrind import Valgrind


class CAPIDriver(BaseDriver):
    TIMEOUT = 300

    #
    # Driver entry poins
    #

    @staticmethod
    def locate_in_path(path_list, filename):
        """Look for `filename` under the directories in `path_list`

        Retrun the absolute path name of such a file if it is found, None
        otherwise. `path_list` must be a string in the same format as the
        *PATH environment variables.
        """
        for path in path_list.split(os.path.pathsep):
            filepath = os.path.join(path, filename)
            if os.path.isfile(filepath):
                return filepath

    @catch_test_errors
    def tear_up(self):
        super(CAPIDriver, self).tear_up()

        if 'compile_units' not in self.test_env:
            raise TestError('Missing "compile_units" key in test.yaml')
        compile_units = self.test_env['compile_units']

        if 'input_sources' not in self.test_env:
            raise TestError('Missing "input_sources" key in test.yaml')
        input_sources = self.test_env['input_sources']

        self.check_file_list(compile_units, can_be_empty=False)
        self.check_file_list(input_sources)
        self.check_file(self.expected_file)

        static_lib = self.locate_in_path(os.environ['LIBRARY_PATH'],
                                         'libadalang.a')
        if not static_lib:
            raise TestError('Could not locate libadalang.a')

        self.gcc_argv = [
            self.global_env['options'].c_compiler,
            '-o', self.test_program, '-g',
            # Be as pedantic as possible, even for testcases.
            '-Wall', '-W', '-Werror', '-pedantic',
        ]
        self.gcc_argv.extend(compile_units)

        # Link with the static library: the resulting program is easier to run
        # out of the test environment.
        self.gcc_argv.append(static_lib)

        # Put stdc++ link option at the end of the link command line so that
        # c++ symbols can be found by the libadalang library. Likewise for the
        # math library (needed by some std::unordered_map instantiations).
        self.gcc_argv.append('-lstdc++')
        self.gcc_argv.append('-lm')

    @catch_test_errors
    def run(self):
        # Build the test program and then run it
        self.run_and_check(self.gcc_argv)
        self.run_and_check([self.test_program], for_debug=True)

    #
    # Helpers
    #

    def check_file_list(self, file_list, can_be_empty=True):
        """Raise a TestError if `file_list` is not a list of strings

        Also raise an error if it is an empty list while `can_be_empty` is
        False.
        """
        if (not isinstance(file_list, list) or
                (not can_be_empty and len(file_list) == 0) or
                not all(isinstance(cu, basestring) for cu in file_list)):
            empty_msg = 'non-empty '
            raise TestError(
                '"compile_units" must be a {}list of strings'.format(
                    empty_msg))

    @property
    def test_program(self):
        return os.path.join(self.working_dir, self.test_env['test_name'])
