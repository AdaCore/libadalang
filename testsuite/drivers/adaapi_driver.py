from e3.testsuite.driver.classic import TestAbortWithError

from drivers.base_driver import BaseDriver


class AdaAPIDriver(BaseDriver):
    """
    Driver to test the Ada API building and running an Ada test program.

    ``test.yaml`` entries:

    * ``main``: Mandatory, name of the Ada source that is the main of the test
      program.

    * ``args``: Optional list of command-line arguments to pass to the test
      programs.  The default is an empty list.

    * ``status_code``: Optional status code (int) that is expected for the test
      programs execution. The default is 0 (execution successful).

    * ``check_output``: Optional boolean: whether to include the test programs
      output to the test output (for baseline comparison). The default is True.
    """

    def run(self):
        # Get the list of mains
        mains = self.test_env['main']
        if isinstance(mains, str):
            mains = [mains]

        # Compute the list of test programs to run
        test_programs = []
        for m in mains:
            ext = ".adb"
            if not m.endswith(ext):
                raise TestAbortWithError(f"Invalid Ada source name: {m}")
            exe_name = m[:-len(ext)] + self.env.build.os.exeext
            test_programs.append(self.working_dir(exe_name))

        argv = self.test_env.get('argv', [])

        status_code = self.test_env.get('status_code', 0)

        append_output = self.test_env.get('check_output', True)

        with open(self.working_dir('gen.gpr'), 'w') as f:
            f.write(f"""
            with "libadalang";

            project Gen is
                for Languages use ("Ada");
                for Source_Dirs use (".");
                for Object_Dir use ".";
                for Main use ({', '.join(f'"{m}"' for m in mains)});

                package Compiler is
                    for Default_Switches ("Ada") use
                      ("-g", "-O0", "-gnata", "-gnatwa", "-gnatX");
                end Compiler;
            end Gen;
            """)

        # Build the test programs and then run them
        self.run_and_check(
            ['gprbuild', '-Pgen'] + self.gpr_scenario_vars,
            append_output=False,
        )
        for i, (m, tp) in enumerate(zip(mains, test_programs)):

            # If there are multiple test programs to run, print a separator to
            # distinguish their outputs in the baseline.
            if i > 0:
                self.output += "\n\n"
            if len(test_programs) > 1:
                self.output += f"== {m} ==\n\n"

            self.run_and_check(
                [tp] + argv,
                memcheck=True,
                status_code=status_code,
                append_output=append_output,
            )
