from __future__ import absolute_import, division, print_function

from testsuite_support.base_driver import BaseDriver, catch_test_errors


class AdaAPIDriver(BaseDriver):

    #
    # Driver entry poins
    #

    @catch_test_errors
    def tear_up(self):
        super(AdaAPIDriver, self).tear_up()

        self.main = self.test_env['main']
        self.input_sources = self.test_env.get('input_sources', [])
        self.argv = self.test_env.get('argv', []) + self.input_sources
        self.status_code = self.test_env.get('status_code', 0)

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
            '''.format(main_source=self.main,
                       exec_name=self.test_program))

    @catch_test_errors
    def run(self):
        # Build the test program and then run it. Whether we use static or
        # shared libraries, make it explicit: some dependencies (such as
        # GNATcoll) use shared ones by default while others such as gnat_util
        # use static ones.
        argv = ['gprbuild', '-Pgen'] + self.gpr_scenario_vars
        self.run_and_check(argv, append_output=False)
        self.run_and_check([self.test_program] + self.argv,
                           for_debug=True, memcheck=True,
                           status_code=self.status_code)

    #
    # Helpers
    #

    @property
    def test_program(self):
        """Return the absolute path to the program to run for this testcase."""
        return self.working_dir(self.test_env['test_name'])
