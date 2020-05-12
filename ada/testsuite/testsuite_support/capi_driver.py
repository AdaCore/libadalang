import os.path

from e3.testsuite.driver.classic import TestAbortWithError

from testsuite_support.base_driver import BaseDriver


class CAPIDriver(BaseDriver):

    @staticmethod
    def locate_in_path(path_list, filename):
        """Look for `filename` under the directories in `path_list`.

        Return the absolute path name of such a file if it is found, None
        otherwise. `path_list` must be a string in the same format as the
        *PATH environment variables.
        """
        for path in path_list.split(os.path.pathsep):
            filepath = os.path.join(path, filename)
            if os.path.isfile(filepath):
                return filepath

    def run(self):
        if 'compile_units' not in self.test_env:
            raise TestAbortWithError(
                'Missing "compile_units" key in test.yaml'
            )
        compile_units = self.test_env['compile_units']

        if 'input_sources' not in self.test_env:
            raise TestAbortWithError(
                'Missing "input_sources" key in test.yaml'
            )
        input_sources = self.test_env['input_sources']

        self.check_file_list('"compile_units"', compile_units,
                             can_be_empty=False)
        self.check_file_list('"input_sources"', input_sources)

        with open(self.working_dir('p.gpr'), 'w') as f:
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
                      ("-Wall", "-W", "-Werror", "-pedantic",

                       "-Wno-error=unused-variable",
                       "-Wno-error=unused-function",
                       --  Code and data are shared in headers, so we expect
                       --  unused variables there.

                       "-std=c99",
                       --  Use a stable C standard. Different compilers means
                       --  different default standard.

                       "-I{support_include_dir}", "-g");
                end Compiler;
            end P;
            '''.format(main_source=compile_units[0],
                       exec_name=self.test_program,
                       support_include_dir=self.support_include_dir))

        # Build the test program and then run it. Whether we use static or
        # shared libraries, make it explicit: some dependencies (such as
        # GNATcoll) use shared ones by default while others such as gnat_util
        # use static ones.
        argv = ['gprbuild', '-Pp'] + self.gpr_scenario_vars
        self.run_and_check(argv, append_output=False)
        self.run_and_check([self.test_program], memcheck=True)

    @property
    def test_program(self):
        """Return the absolute path to the program to run for this testcase."""
        return self.working_dir(self.test_env['test_name'])

    @property
    def support_include_dir(self):
        """
        Return the absolute path to the directory for support C header files.

        These header files are used to share common code among C testcases.
        """
        return os.path.join(self.testsuite_dir, 'c_support')
