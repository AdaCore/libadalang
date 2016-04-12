#! /usr/bin/env python

import glob
import os.path
import shutil
import subprocess
import sys

# Set the environment
from env import setenv
setenv()

from langkit.compile_context import global_context
import langkit.compiled_types as ct
from langkit.libmanage import ManageScript
import langkit.names as names
from langkit.utils import Colors, dispatch_on_type, printcol


class Manage(ManageScript):

    ENABLE_WARNINGS_DEFAULT = True

    def __init__(self):
        super(Manage, self).__init__()

        ########
        # Test #
        ########

        self.test_parser = test_parser = self.subparsers.add_parser(
            'test', help=self.do_test.__doc__
        )
        test_parser.add_argument(
            '--with-gnatpython', '-g', action='store_true',
            dest='with_gnatpython', default=False,
            help='Try to use GNATpython in the testsuite'
        )
        test_parser.add_argument(
            '--without-gnatpython', '-G', action='store_false',
            dest='with_gnatpython',
            help='Do not use GNATpython in the testsuite'
        )
        test_parser.add_argument(
            'testsuite-args', nargs='*',
            help='Arguments to pass to testsuite.py.'
        )
        test_parser.set_defaults(func=self.do_test)

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
        perf_test_parser.set_defaults(func=self.do_perf_test)
        self.add_generate_args(perf_test_parser)
        self.add_build_args(perf_test_parser)

        # Additions to inherited subcommands

        for subparser in (self.generate_parser,
                          self.build_parser,
                          self.make_parser,
                          self.perf_test_parser):
            subparser.add_argument(
                '--no-asteval', action='store_true',
                help='Disable generation/build of the ASTEval subprogram.'
                     ' This can be handy to reduce build time during'
                     ' development.'
            )

    def create_context(self, args):
        # Keep these import statements here so that they are executed only
        # after the coverage computation actually started.
        from langkit.compile_context import CompileCtx
        from language.lexer import ada_lexer
        from language.grammar import ada_grammar

        return CompileCtx(lang_name='Ada',
                          main_rule_name='compilation_unit',
                          lexer=ada_lexer,
                          grammar=ada_grammar,
                          default_charset='iso-8859-1',
                          verbosity=args.verbosity)

    def do_test(self, args):
        """
        Run the testsuite.

        This is a wrapper around testsuite/testsuite.py tuned for interactive
        use: it correctly setups the environment according to the build
        directory, it enables colored output and it displays test outputs on
        error.
        """

        # Make builds available from testcases
        env = self.derived_env()

        if not args.with_gnatpython:
            env['WITHOUT_GNATPYTHON'] = '1'

        argv = [
            'python',
            self.dirs.lang_source_dir('testsuite', 'testsuite.py'),
            '--enable-color', '--show-error-output',
        ]
        if not args.enable_shared:
            argv.append('--disable-shared')
        argv.extend(getattr(args, 'testsuite-args'))

        try:
            subprocess.check_call(argv, env=env)
        except subprocess.CalledProcessError as exc:
            print >> sys.stderr, 'Testsuite failed: {}'.format(exc)
            sys.exit(1)
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

        def file_lines(filename):
            with open(filename) as f:
                return len(list(f))

        work_dir = os.path.abspath(args.work_dir)

        if not args.no_recompile:
            # Build libadalang in production mode inside of the perf testsuite
            # directory.
            args.build_dir = os.path.join(work_dir, 'build')
            self.dirs.set_build_dir(args.build_dir)
            args.build_mode = 'prod'
            self._mkdir(args.build_dir)
            self.do_make(args)

        # Checkout the code bases that we will use for the perf testsuite
        os.chdir(work_dir)
        if not os.path.exists('gnat'):
            subprocess.check_call([
                'svn', 'co',
                'svn+ssh://svn.us.adacore.com/Dev/trunk/gnat',
                '-r', '314163'])
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
            self._find_ada_sources(work_dir)
        )
        file_list_name = 'ada_file_list'
        with open(file_list_name, 'w') as file_list:
            for f in ada_files:
                file_list.write(f + '\n')

        # Get a count of the total number of ada source lines
        lines_count = sum(map(file_lines, ada_files))

        printcol("=================================", Colors.HEADER)
        printcol("= Performance testsuite results =", Colors.HEADER)
        printcol("=================================", Colors.HEADER)
        elapsed_list = []
        for _ in range(args.nb_runs):
            # Execute parse on the file list and get the elapsed time
            t = time()
            subprocess.check_call(['build/bin/parse', '-s', '-F',
                                   file_list_name])
            elapsed = time() - t
            elapsed_list.append(elapsed)

            # Print a very basic report
            print "Parsed {0} lines of Ada code in {1:.2f} seconds".format(
                lines_count, elapsed
            )

        print ""
        printcol("= Performance summary =", Colors.OKGREEN)
        print "Mean time to parse {0} lines of code : {1:.2f} seconds".format(
            lines_count, sum(elapsed_list) / float(len(elapsed_list))
        )

    def do_generate_asteval(self, args):
        """
        Generate sources for the ASTEval test program.
        """
        printcol("Compiling grammar...", Colors.HEADER)
        self.context.compile()

        printcol("Generating ASTEval source files...", Colors.HEADER)

        # Copy Ada sources to the build tree
        src_dir = self.dirs.lang_source_dir('asteval')
        for filename in ([os.path.join(src_dir, 'asteval.gpr')] +
                         glob.glob(os.path.join(src_dir, '*.ad?'))):
            shutil.copyfile(
                filename,
                self.dirs.build_dir('src', os.path.basename(filename))
            )

        def enum_for_type(cls):
            """
            Return the enumerator name corresponding to `cls` (see Value_Kind
            in Mako templates).

            :param ct.CompiledType cls: Type parameter.
            :rtype: str
            """
            name = cls.name().camel_with_underscores
            return dispatch_on_type(cls, [
                (ct.BoolType, lambda _: 'Boolean_Value'),
                (ct.LongType, lambda _: 'Integer_Value'),
                (ct.ASTNode, lambda _: 'Ada_Node_Value'),
                (ct.Token, lambda _: 'Token_Value'),
                (ct.EnumType, lambda _: 'Enum_{}_Value'),
                (ct.Struct, lambda _: 'Struct_{}_Value'),
                (ct.ArrayType, lambda _: 'Array_{}_Value'),
                (ct.LexicalEnvType, lambda _: 'Lexical_Env_Value'),
                (ct.Symbol, lambda _: 'Symbol_Value'),
            ], exception_msg='Unhandled type: {}'.format(cls)).format(name)

        def field_for_type(cls):
            """
            Return the field name corresponding to `cls` (see Value_Type in
            Make templates).

            :param ct.CompiledType cls: Type parameter.
            :rtype: str
            """
            name = cls.name().camel_with_underscores
            return dispatch_on_type(cls, [
                (ct.BoolType, lambda _: 'Bool'),
                (ct.LongType, lambda _: 'Int'),
                (ct.ASTNode, lambda _: 'Node'),
                (ct.Token, lambda _: 'Index'),
                (ct.EnumType, lambda _: 'Enum_{}'),
                (ct.Struct, lambda _: 'Struct_{}'),
                (ct.ArrayType, lambda _: 'Array_{}'),
                (ct.LexicalEnvType, lambda _: 'Lexical_Env'),
                (ct.Symbol, lambda _: 'Symbol'),
            ], exception_msg='Unhandled type: {}'.format(cls)).format(name)

        ctx = self.context

        # List of all types the expression DSL is able to deal with at
        # evaluation time.
        eval_types = (ctx.sorted_types(ctx.enum_types) +
                      ctx.sorted_types(ctx.struct_types) +
                      ctx.sorted_types(ctx.array_types))

        # Generate sources from Mako templates
        with global_context(self.context), names.camel_with_underscores:
            for tmpl in glob.glob(os.path.join(src_dir, '*.mako')):
                src_file, _ = os.path.splitext(tmpl)
                src_file_basename = os.path.basename(src_file)
                with open(self.dirs.build_dir('src', src_file_basename),
                          'w') as f:
                    f.write(ct.render(src_file,
                                      enum_for_type=enum_for_type,
                                      field_for_type=field_for_type,
                                      eval_types=eval_types))

        printcol("ASTEval generation complete!", Colors.HEADER)

    def do_build_asteval(self, args):
        """
        Build the ASTEval program.
        """
        printcol("Building ASTEval...", Colors.HEADER)
        self.gprbuild(args, self.dirs.build_dir('src', 'asteval.gpr'), False)
        printcol("ASTEval build complete!", Colors.HEADER)

    def do_generate(self, args):
        """
        Generate the Libadalang and Langkit libraries. Also generate the
        ASTEval test program.
        """
        super(Manage, self).do_generate(args)
        if not args.no_asteval:
            self.do_generate_asteval(args)

    def do_build(self, args):
        """
        Build the Libadalang and Langkit generated libraries. Also generate and
        build the ASTEval test program.
        """
        super(Manage, self).do_build(args)
        if not args.no_asteval:
            self.do_build_asteval(args)


if __name__ == '__main__':
    Manage().run()
