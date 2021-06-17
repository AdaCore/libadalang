from e3.fs import sync_tree
from e3.testsuite.driver.classic import TestSkip

from drivers.base_driver import BaseDriver


class OCamlDriver(BaseDriver):
    """
    Build and run "test.ml" against the Libadalang OCaml bindings.
    """

    main_module = 'test'
    """
    Name of the main module for the program to build and run.
    """

    def run(self):
        bindings_dir = self.env.ocaml_bindings
        if not bindings_dir:
            raise TestSkip('Test requires OCaml')

        # Copy sources for the test program and setup a Dune project for it
        with open(self.working_dir('dune'), 'w') as f:
            f.write("""
                (executable
                  (name {})
                  (flags (-w -9))
                  (libraries str libadalang))
            """.format(self.main_module))
        with open(self.working_dir('dune-project'), 'w') as f:
            f.write('(lang dune 1.6)')

        # Copy the OCaml bindings: for now, we consider it's the job of the
        # testcase to build it.
        sync_tree(bindings_dir, self.working_dir('bindings'))

        self.run_and_check(['dune', 'exec',
                            '--display', 'quiet',
                            '--root', '.',
                            './{}.exe'.format(self.main_module)])
