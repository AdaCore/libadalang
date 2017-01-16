from testsuite_support.base_driver import (
    BaseDriver, catch_test_errors, SetupError,
)


class CAPIDriver(BaseDriver):
    TIMEOUT = 300

    #
    # Driver entry poins
    #

    @catch_test_errors
    def tear_up(self):
        super(CAPIDriver, self).tear_up()

        if 'main' not in self.test_env:
            raise SetupError('Missing "main" key in test.yaml')
        main = self.test_env['main']

        if 'input_sources' not in self.test_env:
            raise SetupError('Missing "input_sources" key in test.yaml')
        input_sources = self.test_env['input_sources']

        self.check_file(main)
        self.check_file_list('"input_sources"', input_sources)

        with open(self.working_dir('gen.gpr'), 'w') as f:
            f.write('''
            with "libadalang";

            project Gen is
                for Languages use ("Ada");
                for Source_Dirs use (".");
                for Object_Dir use ".";
                for Main use ("{main_source}");

                package Builder is
                    for Executable ("{main_source}") use "{exec_name}";
                end Builder;

                package Compiler is
                    for Default_Switches ("Ada") use
                      ("-g", "-O0", "-gnata", "-gnatwa");
                end Compiler;
            end Gen;
            '''.format(main_source=main,
                       exec_name=self.test_program))

    @catch_test_errors
    def run(self):
        # Build the test program and then run it. Whether we use static or
        # shared libraries, make it explicit: some dependencies (such as
        # GNATcoll) use shared ones by default while others such as gnat_util
        # use static ones.
        argv = ['gprbuild', '-Pgen', '-m']
        if self.disable_shared:
            argv.append('-XLIBRARY_TYPE=static')
        else:
            argv.append('-XLIBRARY_TYPE=relocatable')
        self.run_and_check(argv, append_output=False)
        self.run_and_check([self.test_program], for_debug=True, memcheck=True)

    #
    # Helpers
    #

    @property
    def test_program(self):
        """Return the absolute path to the program to run for this testcase."""
        return self.working_dir(self.test_env['test_name'])
