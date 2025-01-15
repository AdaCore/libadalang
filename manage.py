#! /usr/bin/env python

import os.path
import sys


# For developer convenience, add the "langkit" directory next to this script to
# the Python path so that.
sys.path.append(
    os.path.join(os.path.dirname(os.path.abspath(__file__)), 'langkit')
)


from langkit.libmanage import ManageScript
from langkit.utils import LibraryType


class Manage(ManageScript):

    def add_extra_subcommands(self) -> None:
        ########
        # Test #
        ########

        self.test_parser = self.add_subcommand(
            self.do_test, needs_context=True, accept_unknown_args=True
        )
        self.test_parser.add_argument(
            '--disable-ocaml', action='store_true',
            help='Disable tests involving the OCaml API'
        )
        self.test_parser.add_argument(
            '--disable-java', action="store_true",
             help='Disable tests involving the Java API'
        )
        self.test_parser.add_argument(
            'testsuite-args', nargs='*',
            help='Arguments to pass to testsuite.py.'
        )
        self.add_build_mode_arg(self.test_parser)

        #########
        # ACATS #
        #########

        self.acats_parser = self.add_subcommand(
            self.do_acats, needs_context=True, accept_unknown_args=False
        )
        self.acats_parser.add_argument(
            '--acats-dir', default='acats',
            help='The path to the acats repository. By default, use "acats" in'
                 ' the current directory.'
        )
        self.acats_parser.add_argument(
            'acats-args', nargs='*',
            help='Arguments to pass to run_acats_test.py.'
        )
        self.add_build_mode_arg(self.acats_parser)

    def create_config(self, args):
        return self.load_yaml_config(
            os.path.join(os.path.dirname(__file__), "langkit.yaml")
        )

    def do_test(self, args, unknown_args):
        """
        Run the testsuite.

        This is a wrapper around testsuite/testsuite.py tuned for interactive
        use: it correctly setups the environment according to the build
        directory, it enables colored output and it displays test outputs on
        error.
        """
        argv = [
            sys.executable,
            self.dirs.lang_source_dir('testsuite', 'testsuite.py'),
            '-Edtmp', '--build-mode={}'.format(args.build_modes[0].name),

            # Arguments to pass to GNATcoverage, just in case coverage is
            # requested.
            '--gnatcov-instr-dir={}'.format(
                os.path.join(args.build_dir, 'obj', 'libadalang', 'instr')
            )
        ]
        if not args.disable_ocaml:
            argv.append('--with-ocaml-bindings')
            argv.append(os.path.join(args.build_dir, 'ocaml'))

        if not args.disable_java:
            argv.append('--with-java-bindings')
            argv.append(os.path.join(args.build_dir, 'java'))

        if not LibraryType.relocatable in args.library_types:
            argv.append('--disable-shared')

        argv.extend(unknown_args)
        argv.extend(getattr(args, 'testsuite-args'))

        try:
            return self.check_call('Testsuite', argv, direct_c_header=True)
        except KeyboardInterrupt:
            # At this point, the testsuite already made it explicit we stopped
            # after a keyboard interrupt, so we just have to exit.
            sys.exit(1)

    def do_acats(self, args):
        """
        Run the ACATS testsuite.

        This is a wrapper around run_acats_test.py of the libadalang part of
        the ACATS testsuite.
        """
        path = args.acats_dir

        argv = [
            sys.executable,
            self.dirs.lang_source_dir(path, 'run_acats_test.py'),
            '--acats-dir', path,
            '--mode=libadalang'
        ]

        argv.extend(getattr(args, 'acats-args'))

        try:
            return self.check_call('ACATS', argv)
        except KeyboardInterrupt:
            sys.exit(1)


if __name__ == '__main__':
    Manage().run()
