#! /usr/bin/env python

import os.path
import subprocess
import sys

# Set the environment
from env import setenv
setenv()

from langkit.libmanage import ManageScript, get_cpu_count
from langkit.utils import Colors, printcol


class Manage(ManageScript):

    def __init__(self):
        super(Manage, self).__init__()

        ########
        # Test #
        ########

        self.test_parser = test_parser = self.subparsers.add_parser(
            'test', help=self.do_test.__doc__
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
            '--jobs', '-j', type=int, default=get_cpu_count(),
            help='Number of parallel jobs to spawn in parallel'
                 ' (default: your number of cpu)'
        )
        perf_test_parser.add_argument(
            '--work-dir', '-w', default='performance_testsuite',
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

    def create_context(self, args):
        # Keep these import statements here so that they are executed only
        # after the coverage computation actually started.
        from langkit.compile_context import CompileCtx
        from language.parser import ada_lexer, ada_grammar

        return CompileCtx(lang_name='Ada',
                          main_rule_name='compilation_unit',
                          lexer=ada_lexer,
                          grammar=ada_grammar,
                          verbose=args.verbose)

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
        argv = [
            'python',
            self.dirs.lang_source_dir('testsuite', 'testsuite.py'),
            '--enable-color', '--show-error-output',
        ]
        if args.disable_shared:
            argv.append('--disable-shared')
        argv.extend(getattr(args, 'testsuite-args'))

        try:
            subprocess.check_call(argv, env=env)
        except subprocess.CalledProcessError as exc:
            print >> sys.stderr, 'Testsuite failed: {}'.format(exc)
            sys.exit(1)

    @staticmethod
    def _mkdir(path):
        """
        Create a new directory at `path` if it does not exist

        :param path: the path to the new directory
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
        Return the list of .adb and .ads files in `work_dir`

        :param work_dir: the directory in which to search for ada sources
        :type work_dir: str
        :return: set[str]
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
        Run the performance regression testsuite
        """
        from time import time

        def file_lines(filename):
            with open(filename) as f:
                return len(list(f))

        work_dir = os.path.abspath(args.work_dir)

        if not args.no_recompile:
            # Build libadalang in production mode inside of the perf testsuite
            # directory
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
        # correctly
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


if __name__ == '__main__':
    Manage().run()
