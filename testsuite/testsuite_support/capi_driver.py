import os.path
import pipes

from gnatpython import fileutils
from gnatpython.ex import Run, STDOUT

from testsuite_support.base_driver import (
    BaseDriver, catch_test_errors, SetupError,
)


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
            raise SetupError('Missing "compile_units" key in test.yaml')
        compile_units = self.test_env['compile_units']

        if 'input_sources' not in self.test_env:
            raise SetupError('Missing "input_sources" key in test.yaml')
        input_sources = self.test_env['input_sources']

        self.check_file_list('"compile_units"', compile_units,
                             can_be_empty=False)
        self.check_file_list('"input_sources"', input_sources)

        with open(os.path.join(self.working_dir, 'p.gpr'), 'w') as f:
            f.write('''
            with "libadalang";

            project P is
                for Languages use ("C");
                for Source_Dirs use (".");
                for Object_Dir use ".";
                for Main use ("{main_source}");

                package Builder is
                    for Executable ("{main_source}") use "{exec_name}";
                end Builder;

                package Compiler is
                    for Default_Switches ("C") use
                      ("-Wall", "-W", "-Werror", "-pedantic");
                end Compiler;
            end P;
            '''.format(main_source=compile_units[0],
                       exec_name=self.test_program))

    @catch_test_errors
    def run(self):
        # Build the test program and then run it. Whether we use static or
        # shared libraries, make it explicit: some dependencies (such as
        # GNATcoll) use shared ones by default while others such as gnat_util
        # use static ones.
        argv = ['gprbuild', '-Pp']
        if self.disable_shared:
            argv.append('-XLIBRARY_TYPE=static')
        else:
            argv.append('-XLIBRARY_TYPE=relocatable')
        self.run_and_check(argv)
        self.run_and_check([self.test_program], for_debug=True, memcheck=True)

    #
    # Helpers
    #

    @property
    def test_program(self):
        return os.path.join(self.working_dir, self.test_env['test_name'])
