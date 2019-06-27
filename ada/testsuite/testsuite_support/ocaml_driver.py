from __future__ import absolute_import, division, print_function

from testsuite_support.base_driver import (BaseDriver, catch_test_errors,
                                           fileutils)


class OCamlDriver(BaseDriver):
    """
    Build and run "test.ml" against the Libadalang OCaml bindings.
    """

    main_module = 'test'
    """
    Name of the main module for the program to build and run.
    """

    @catch_test_errors
    def tear_up(self):
        super(OCamlDriver, self).tear_up()

        bindings_dir = self.global_env['ocaml_bindings']
        if not bindings_dir:
            self.result.set_status('DEAD', 'Test requires OCaml')
            return

        # Copy sources for the test program and setup a Dune project for it
        with open(self.working_dir('dune'), 'w') as f:
            f.write("""
                (executable
                  (name {})
                  (flags (-w -9))
                  (libraries libadalang))
            """.format(self.main_module))
        with open(self.working_dir('dune-project'), 'w') as f:
            f.write('(lang dune 1.6)')

        # Copy the OCaml bindings: for now, we consider it's the job of the
        # testcase to build it.
        fileutils.sync_tree(bindings_dir, self.working_dir('bindings'))

    @catch_test_errors
    def run(self):
        return self.run_and_check([
            'dune', 'exec',
            '--display', 'quiet',
            '--root', '.',
            './{}.exe'.format(self.main_module)
        ])
