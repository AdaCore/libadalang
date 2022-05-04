from drivers.base_driver import BaseDriver


class AdaAPIDriver(BaseDriver):
    """
    Driver to test the Ada API building and running an Ada test program.

    "test.yaml" entries:

    ``main``

        Mandatory, name of the Ada source that is the main of the test program.

    ``args``

        Optional list of command-line arguments to pass to the test program.
        The default is an empty list.

    ``input_sources``

        Optional list of source files, to be passed as additional command-line
        arguments to the test program. The default is an empty list.

    ``status_code``

        Optional status code (int) that is expected for the test program
        execution. The default is 0 (execution successful).

    ``check_output``

        Optional boolean: whether to include the test program output to the
        test output (for baseline comparison). The default is True.
    """

    @property
    def test_program(self):
        """Return the absolute path to the program to run for this testcase."""
        return self.working_dir(self.test_env['test_name'])

    def run(self):
        main = self.test_env['main']

        input_sources = self.test_env.get('input_sources', [])
        argv = self.test_env.get('argv', []) + input_sources

        status_code = self.test_env.get('status_code', 0)

        append_output = self.test_env.get('check_output', True)

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
            '''.format(main_source=main, exec_name=self.test_program))

        # Build the test program and then run it
        self.run_and_check(['gprbuild', '-Pgen'] + self.gpr_scenario_vars,
                           append_output=False)
        self.run_and_check([self.test_program] + argv,
                           memcheck=True,
                           status_code=status_code,
                           append_output=append_output)
