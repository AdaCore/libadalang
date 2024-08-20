#! /usr/bin/env python

import os.path
import subprocess
import sys
from langkit.utils import LibraryType


# For developer convenience, add the "langkit" directory next to this script to
# the Python path so that.
sys.path.append(
    os.path.join(os.path.dirname(os.path.abspath(__file__)), 'langkit')
)


import ada.copyright

from langkit.diagnostics import check_source_language
from langkit.libmanage import ManageScript
import langkit.names as names
from langkit.utils import Colors, printcol


class Manage(ManageScript):

    ENABLE_BUILD_WARNINGS_DEFAULT = True
    ENABLE_JAVA_DEFAULT = True

    def add_extra_subcommands(self) -> None:
        ########
        # Test #
        ########

        self.test_parser = self.add_subcommand(
            self.do_test, accept_unknown_args=True
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
            self.do_acats, accept_unknown_args=False
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

    def create_context(self, args):
        # Keep these import statements here so that they are executed only
        # after the coverage computation actually started.
        from langkit.compile_context import (
            AdaSourceKind, CacheCollectionConf, CompileCtx, LibraryEntity
        )
        from ada.documentation import libadalang_docs

        # Give Liblktlang access to our Lkt files
        os.environ["LKT_PATH"] = os.path.join(os.path.dirname(__file__), "ada")

        ctx = CompileCtx(
            lang_name='Ada',
            short_name='lal',
            lexer=None,
            grammar=None,
            types_from_lkt=True,
            lkt_file=os.path.join(os.path.dirname(__file__), "ada", "nodes.lkt"),
            default_charset='iso-8859-1',
            verbosity=args.verbosity,
            default_unit_provider=LibraryEntity(
                'Libadalang.Internal_Default_Provider', 'Create'
            ),
            symbol_canonicalizer=LibraryEntity('Libadalang.Sources',
                                               'Canonicalize'),
            documentations=libadalang_docs,
            property_exceptions={"Precondition_Failure"},

            generate_unparser=True,
            default_unparsing_config="default_unparsing_config.json",

            # Setup a configuration of the cache collection mechanism that
            # works well for Ada.
            cache_collection_conf=CacheCollectionConf(
                threshold_increment=100000,
                decision_heuristic=LibraryEntity(
                    "Libadalang.Implementation.Extensions",
                    "Should_Collect_Env_Caches"
                )
            )
        )

        # Internals need to access environment hooks, the symbolizer and
        # internal configuration pragmas file tables.
        ctx.add_with_clause('Implementation',
                            AdaSourceKind.body, 'Libadalang.Env_Hooks',
                            use_clause=True)
        ctx.add_with_clause('Implementation',
                            AdaSourceKind.body, 'Libadalang.Sources',
                            use_clause=False)
        ctx.add_with_clause('Implementation',
                            AdaSourceKind.spec,
                            'Libadalang.Config_Pragmas_Impl',
                            use_clause=True)

        # Bind Libadalang's custom iterators to the public API
        ctx.add_with_clause('Iterators',
                            AdaSourceKind.body,
                            'Libadalang.Iterators.Extensions')

        # LAL.Analysis.Is_Keyword is implemented using LAL.Lexer's
        ctx.add_with_clause('Analysis', AdaSourceKind.body, 'Libadalang.Lexer')

        # LAL.Analysis.Create_Context_From_Project just calls the homonym
        # function in LAL.Project_Provider.
        for dep in [
            "GNATCOLL.Projects", "GPR2.Project.Tree", "GPR2.Project.View"
        ]:
            ctx.add_with_clause("Analysis", AdaSourceKind.spec, dep)
        for dep in ["Libadalang.GPR_Impl", "Libadalang.Project_Provider"]:
            ctx.add_with_clause("Analysis", AdaSourceKind.body, dep)

        # LAL.Lexer.Is_Keyword's implementation uses precomputed symbols
        ctx.add_with_clause('Lexer',
                            AdaSourceKind.body,
                            'Libadalang.Implementation')

        ctx.post_process_ada = ada.copyright.format_ada
        ctx.post_process_cpp = ada.copyright.format_c
        ctx.post_process_python = ada.copyright.format_python
        ctx.post_process_ocaml = ada.copyright.format_ocaml

        # Register our custom exception types
        ctx.register_exception_type(
            package=["GPR2"],
            name=names.Name("Project_Error"),
            doc_section="libadalang.project_provider",
        )
        ctx.register_exception_type(
            package=["Libadalang", "Project_Provider"],
            name=names.Name("Unsupported_View_Error"),
            doc_section="libadalang.project_provider",
        )

        return ctx

    @property
    def main_source_dirs(self):
        return super(Manage, self).main_source_dirs | {
            os.path.join('testsuite', 'ada'),
            os.path.join('testsuite', 'ada', 'gnat_compare')
        }

    @property
    def main_programs(self):
        return super(Manage, self).main_programs | {
            'nameres', 'navigate', 'gnat_compare', 'lal_dda', 'lal_prep',
        }

    def do_generate(self, args):
        # Report unused documentation entries
        args.report_unused_doc_entries = True
        super(Manage, self).do_generate(args)
    do_generate.__doc__ = ManageScript.do_generate.__doc__

    def do_install(self, args):
        from e3.fs import sync_tree

        super(Manage, self).do_install(args)

        # Prepare an "examples" directory
        examples_dir = self.dirs.install_dir(
            "share", "examples", self.lib_name.lower(),
        )
        if not os.path.isdir(examples_dir):
            os.makedirs(examples_dir)

        # Install several items from "contrib" to this directory
        for item in [
            "check_deref_null.py",
            "check_same_logic.py",
            "check_same_operands.py",
            "check_same_test.py",
            "check_same_then_else.py",
            "check_subp_boxes.py",
            "check_test_not_null.py",
            "check_useless_assign.py",
            "detect_copy_paste_sa.py",
            "highlight",
        ]:
            item_from = self.dirs.lang_source_dir("contrib", item)
            item_to = os.path.join(examples_dir, item)
            sync_tree(item_from, item_to, delete=False)

    do_install.__doc__ = ManageScript.do_generate.__doc__

    def do_test(self, args, unknown_args):
        """
        Run the testsuite.

        This is a wrapper around testsuite/testsuite.py tuned for interactive
        use: it correctly setups the environment according to the build
        directory, it enables colored output and it displays test outputs on
        error.
        """
        self.set_context(args)

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
        self.set_context(args)

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

    @staticmethod
    def _mkdir(path):
        """
        Create a new directory at `path` if it does not exist.

        :param path: the path to the new directory.
        :type path: str
        :raise: OSError | IOError
        """

        if os.path.isdir(path):
            return
        if os.path.exists(path):
            raise IOError('{}: already exists'.format(path))
        os.makedirs(path)

    @staticmethod
    def _find_ada_sources(work_dir):
        """
        Return the list of .adb and .ads files in `work_dir`.

        :param work_dir: the directory in which to search for ada sources.
        :type work_dir: str
        :rtype: set[str]
        """
        ada_files = set()
        for root, dirs, files in os.walk(work_dir):
            for filename in files:
                _, ext = os.path.splitext(filename)
                if ext in ('.ads', '.adb'):
                    ada_files.add(os.path.join(root, filename))
        return ada_files


if __name__ == '__main__':
    Manage().run()
