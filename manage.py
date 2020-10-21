#! /usr/bin/env python

import os.path
import subprocess
import sys


# For developer convenience, add the "langkit" directory next to this script to
# the Python path so that.
sys.path.append(
    os.path.join(os.path.dirname(os.path.abspath(__file__)), 'langkit')
)


import ada.copyright

from langkit.diagnostics import check_source_language
from langkit.libmanage import ManageScript
from langkit.utils import Colors, printcol


class Manage(ManageScript):

    ENABLE_BUILD_WARNINGS_DEFAULT = True

    PERF_PARSE = 'parse'
    PERF_PARSE_AND_TRAVERSE = 'parse-and-traverse'
    PERF_CHOICES = (PERF_PARSE, PERF_PARSE_AND_TRAVERSE)

    def __init__(self):
        super(Manage, self).__init__()

        ########
        # Test #
        ########

        self.test_parser = test_parser = self.subparsers.add_parser(
            'test', help=self.do_test.__doc__
        )
        test_parser.add_argument(
            '--disable-ocaml', action='store_true',
            help='Disable tests involving the OCaml API'
        )
        test_parser.add_argument(
            'testsuite-args', nargs='*',
            help='Arguments to pass to testsuite.py.'
        )
        test_parser.set_defaults(func=self.do_test)
        self.add_build_mode_arg(test_parser)

        #############
        # Perf Test #
        #############

        self.perf_test_parser = perf_test_parser = self.subparsers.add_parser(
            'perf-test', help=self.do_perf_test.__doc__
        )
        perf_test_parser.add_argument(
            '--work-dir', default='performance_testsuite',
            help='Directory into which the performance testsuite will be'
                 ' executed'
        )
        perf_test_parser.add_argument(
            '--nb-runs', type=int, default=4,
            help='Number of runs (default: 4)'
        )
        perf_test_parser.add_argument(
            '--no-recompile', action='store_true',
            help='Do not recompile the library before running the perf'
                 ' testsuite'
        )
        perf_test_parser.add_argument(
            '--scenario', '-s',
            choices=self.PERF_CHOICES, default=self.PERF_PARSE,
            help='Profiling scenario to use. Basically: "what to measure?".'
        )
        perf_test_parser.add_argument(
            '--with-trivia', action='store_true',
            help='Include trivia in parsing'
        )
        perf_test_parser.set_defaults(func=self.do_perf_test)
        self.add_generate_args(perf_test_parser)
        self.add_build_args(perf_test_parser)

    def create_context(self, args):
        # Keep these import statements here so that they are executed only
        # after the coverage computation actually started.
        from langkit.compile_context import ADA_BODY, CompileCtx, LibraryEntity
        from ada.lexer import ada_lexer
        from ada.grammar import ada_grammar
        from ada.documentation import libadalang_docs

        ctx = CompileCtx(
            lang_name='Ada',
            short_name='LAL',
            lexer=ada_lexer,
            grammar=ada_grammar,
            default_charset='iso-8859-1',
            verbosity=args.verbosity,
            default_unit_provider=LibraryEntity(
                'Libadalang.Internal_Default_Provider', 'Create'
            ),
            symbol_canonicalizer=LibraryEntity('Libadalang.Sources',
                                               'Canonicalize'),
            documentations=libadalang_docs,
        )

        # Internals need to access environment hooks and the symbolizer
        ctx.add_with_clause('Implementation',
                            ADA_BODY, 'Libadalang.Env_Hooks',
                            use_clause=True)
        ctx.add_with_clause('Implementation',
                            ADA_BODY, 'Libadalang.Sources',
                            use_clause=False)

        # Bind Libadalang's custom iterators to the public API
        ctx.add_with_clause('Iterators',
                            ADA_BODY, 'Libadalang.Iterators.Extensions')

        # LAL.Analysis.Is_Keyword is implemented using LAL.Lexer's
        ctx.add_with_clause('Analysis', ADA_BODY, 'Libadalang.Lexer')

        ctx.post_process_ada = ada.copyright.format_ada
        ctx.post_process_cpp = ada.copyright.format_c
        ctx.post_process_python = ada.copyright.format_python

        return ctx

    @property
    def main_source_dirs(self):
        return super(Manage, self).main_source_dirs | {
            os.path.join('testsuite', 'ada'),
            os.path.join('testsuite', 'ada', 'gnat_compare')
        }

    @property
    def main_programs(self):
        return super(Manage, self).main_programs | {'nameres', 'navigate',
                                                    'gnat_compare'}

    def do_generate(self, args):
        # Always generate the unparsing machinery and report unused
        # documentation entries.
        args.generate_unparser = True
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

    def do_test(self, args):
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
            '-Edtmp', '--build-mode={}'.format(args.build_mode),

            # Arguments to pass to GNATcoverage, just in case coverage is
            # requested.
            '--gnatcov-instr-dir={}'.format(
                os.path.join(args.build_dir, 'obj', 'libadalang', 'instr')
            )
        ]
        if not args.disable_ocaml:
            argv.append('--with-ocaml-bindings')
            argv.append(os.path.join('build', 'ocaml'))
        if not args.library_types.relocatable:
            argv.append('--disable-shared')
        argv.extend(getattr(args, 'testsuite-args'))

        try:
            return self.check_call(args, 'Testsuite', argv)
        except KeyboardInterrupt:
            # At this point, the testsuite already made it explicit we stopped
            # after a keyboard interrupt, so we just have to exit.
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

    def do_perf_test(self, args):
        """
        Run the performance regression testsuite.
        """
        from time import time

        self.set_context(args)

        def file_lines(filename):
            with open(filename) as f:
                return len(list(f))

        check_source_language(
            not os.path.isabs(args.build_dir),
            "--build-dir should be a relative path for perf testsuite"
        )

        work_dir = os.path.abspath(args.work_dir)
        variant_name = args.build_dir
        report_file = os.path.join(work_dir,
                                   'report-{}.txt'.format(variant_name))
        args.build_dir = os.path.join(work_dir, args.build_dir)

        if not args.no_recompile:
            # The perf testsuite only needs the "parse" main program
            args.disable_mains = self.main_programs - {'parse'}

            # Build libadalang in production mode inside of the perf testsuite
            # directory.
            self.dirs.set_build_dir(args.build_dir)
            args.build_mode = 'prod'
            self._mkdir(args.build_dir)
            self.do_make(args)

        # Checkout the code bases that we will use for the perf testsuite
        source_dir = os.path.join(work_dir, "source")
        try:
            os.mkdir(source_dir)
        except OSError:
            pass
        os.chdir(source_dir)
        if not os.path.exists('gnat'):
            subprocess.check_call([
                'svn', 'co',
                'svn+ssh://svn.us.adacore.com/Dev/trunk/gnat',
                '-r', '314163',
                '--ignore-externals'
            ])
        if not os.path.exists('gps'):
            subprocess.check_call(['git', 'clone',
                                   'ssh://review.eu.adacore.com:29418/gps'])
        os.chdir('gps')
        subprocess.check_call(['git', 'checkout',
                               '00b73897a867514732d48ae1429faf97fb07ad7c'])
        os.chdir('..')

        # Make a list of every ada file

        # Exclude some files that are contained here but that we do not parse
        # correctly.
        excluded_patterns = ['@', 'a-numeri', 'rad-project']
        ada_files = filter(
            lambda f: all(map(lambda p: p not in f, excluded_patterns)),
            self._find_ada_sources(source_dir)
        )
        file_list_name = 'ada_file_list'
        with open(file_list_name, 'w') as file_list:
            for f in ada_files:
                file_list.write(f + '\n')

        # Get a count of the total number of ada source lines
        lines_count = sum(map(file_lines, ada_files))

        with open(report_file, 'w') as f:
            def write_report(text, color=None):
                if color:
                    printcol(text, color)
                else:
                    print(text)
                print(text, file=f)

            write_report('=================================', Colors.HEADER)
            write_report('= Performance testsuite results =', Colors.HEADER)
            write_report('=================================', Colors.HEADER)
            write_report('')
            write_report('Name: {}'.format(variant_name))
            write_report('Scenario: {}'.format(args.scenario))
            write_report('')
            elapsed_list = []
            parse_args = ['{}/bin/parse'.format(args.build_dir), '-s', '-F',
                          file_list_name]
            if args.scenario == self.PERF_PARSE_AND_TRAVERSE:
                parse_args.append('-C')
            if args.with_trivia:
                parse_args.append('-P')
            for _ in range(args.nb_runs):
                # Execute parse on the file list and get the elapsed time
                t = time()
                subprocess.check_call(parse_args)
                elapsed = time() - t
                elapsed_list.append(elapsed)

                # Print a very basic report
                write_report(
                    'Parsed {0} lines of Ada code in {1:.2f} seconds'.format(
                        lines_count, elapsed
                    )
                )

            write_report('')
            write_report('= Performance summary =', Colors.OKGREEN)
            write_report(
                'Mean time to parse {0} lines of code:'
                ' {1:.2f} seconds'.format(
                    lines_count, sum(elapsed_list) / float(len(elapsed_list))
                )
            )


if __name__ == '__main__':
    Manage().run()
