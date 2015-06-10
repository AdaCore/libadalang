#! /usr/bin/env python

import argparse
import glob
import os.path
import pipes
import shutil
import subprocess
import sys

# Set the environment
from env import setenv
setenv()

from gnatpython import fileutils
from ada_api import AdaAPISettings
from c_api import CAPISettings
from compile_context import CompileCtx
from python_api import PythonAPISettings
from utils import Colors, printcol


class Directories(object):
    """
    Helper class used to get various path in source/build/install trees.
    """

    def __init__(self, args):
        self.args = args
        self.root_dir = os.path.dirname(os.path.dirname(
            os.path.abspath(__file__)
        ))

    @property
    def root_source_dir(self):
        return self.root_dir

    @property
    def root_build_dir(self):
        return os.path.abspath(self.args.build_dir)

    @property
    def root_install_dir(self):
        return os.path.abspath(getattr(self.args, 'install-dir'))

    def source_dir(self, *args):
        return os.path.join(self.root_source_dir, *args)

    def build_dir(self, *args):
        return os.path.join(self.root_build_dir, *args)

    def install_dir(self, *args):
        return os.path.join(self.root_install_dir, *args)


class Coverage(object):
    """
    Guard object used to compute code coverage (optionally).
    """

    def __init__(self, dirs):
        self.dirs = dirs

        import coverage
        self.cov = coverage.coverage(
            branch=True,
            source=[
                self.dirs.source_dir(),
                self.dirs.source_dir('ada'),
            ],
            omit=[
                self.dirs.source_dir('compiler', 'build.py'),
                self.dirs.source_dir('compiler', 'env.py'),
            ],
        )

        self.cov.exclude('def __repr__')
        self.cov.exclude('raise NotImplementedError()')
        self.cov.exclude('assert False')

    def __enter__(self):
        self.cov.start()

    def __exit__(self, value, type, traceback):
        del value, type, traceback
        self.cov.stop()

    def generate_report(self):
        self.cov.html_report(
            directory=self.dirs.build_dir('coverage'),
            ignore_errors=True
        )


def get_cpu_count():
    # The "multiprocessing" module is not available on GNATpython's
    # distribution for PPC AIX and the "cpu_count" is not available on Windows:
    # give up on default parallelism on these platforms.
    try:
        import multiprocessing
    except ImportError:
        return 1

    try:
        return multiprocessing.cpu_count()
    except NotImplementedError:
        return 1


def generate(args, dirs):
    """Generate source code for libadalang."""
    lexer_file = dirs.source_dir('ada', 'ada.qx')
    ada_api_settings = AdaAPISettings('Libadalang')
    c_api_settings = CAPISettings(
        'libadalang',
        symbol_prefix='ada',
    )
    python_api_settings = (PythonAPISettings('libadalang', c_api_settings)
                           if 'python' in args.bindings else None)
    context = CompileCtx('ada', lexer_file, 'compilation_unit',
                         ada_api_settings,
                         c_api_settings,
                         python_api_settings,
                         verbose=args.verbose)

    from ada_parser import A
    import ada_parser.decl
    import ada_parser.types
    import ada_parser.exprs
    import ada_parser.bodies

    del ada_parser

    context.set_grammar(A)

    printcol("Generating source for libadalang ...", Colors.HEADER)
    context.emit(file_root=dirs.build_dir())

    def gnatpp(project_file):
        try:
            subprocess.check_call([
                'gnatpp',
                '-P{}'.format(project_file),
                '-XLIBRARY_TYPE=relocatable',
                '-rnb',
            ], env=derived_env(dirs))
        except subprocess.CalledProcessError as exc:
            print >> sys.stderr, 'Pretty-printing failed: {}'.format(exc)
            sys.exit(1)

    if hasattr(args, 'pretty_print') and args.pretty_print:
        printcol("Pretty-printing sources for libadalang ...", Colors.HEADER)
        gnatpp(dirs.build_dir('lib', 'gnat', 'libadalang.gpr'))
        gnatpp(dirs.build_dir('src', 'parse.gpr'))

    printcol("Generation complete !", Colors.OKGREEN)


BUILD_MODES = {
    'dev': ['-g', '-O0'],
    # Debug information is useful even with optimization for profiling, for
    # instance.
    'prod': ['-g', '-Ofast', '-cargs:Ada', '-gnatp'],
}


def build(args, dirs):
    """Build generated source code."""

    cargs = []
    if args.build_mode:
        cargs.extend(BUILD_MODES[args.build_mode])

    # Depending on where this is invoked, the "cargs" option may not be set
    if hasattr(args, 'cargs'):
        cargs.extend(args.cargs)

    def gprbuild(project_file, is_dynamic):
        try:
            subprocess.check_call([
                'gprbuild', '-p', '-j{}'.format(args.jobs),
                '-P{}'.format(project_file),
                '-XLIBRARY_TYPE={}'.format(
                    'relocatable' if is_dynamic else 'static'
                ),
                '-XLIBLANG_SUPPORT_EXTERNALLY_BUILT=false',
                '-XLIBADALANG_EXTERNALLY_BUILT=false',
                '-cargs',
            ] + cargs, env=derived_env(dirs))
        except subprocess.CalledProcessError as exc:
            print >> sys.stderr, 'Build failed: {}'.format(exc)
            sys.exit(1)

    printcol("Building the generated source code ...", Colors.HEADER)
    if args.enable_static:
        gprbuild(dirs.build_dir('lib', 'gnat', 'libadalang.gpr'), False)
    if not args.disable_shared:
        gprbuild(dirs.build_dir('lib', 'gnat', 'libadalang.gpr'), True)

    printcol("Building the interactive test main ...", Colors.HEADER)
    if args.enable_static:
        gprbuild(dirs.build_dir('src', 'parse.gpr'), False)
    if not args.disable_shared:
        gprbuild(dirs.build_dir('src', 'parse.gpr'), True)

    # On Windows, shared libraries (DLL) are looked up in the PATH, just like
    # binaries (it's LD_LIBRARY_PATH on Unix). For this platform, don't bother
    # and just copy these DLL next to binaries.
    if os.name == 'nt':
        for dll in glob.glob(dirs.build_dir('lib', '*.dll')):
            shutil.copy(dll, dirs.build_dir('bin', os.path.basename(dll)))

    printcol("Compilation complete!", Colors.OKGREEN)


def make(args, dirs):
    """Generate and build in one command"""
    generate(args, dirs)
    build(args, dirs)


def install(args, dirs):
    """Install programs and libraries."""
    del args

    for subdir in ('bin', 'include', 'lib', 'python'):
        fileutils.sync_tree(
            dirs.build_dir(subdir),
            dirs.install_dir(subdir)
        )


def setup_environment(dirs, add_path):
    add_path('PATH', dirs.build_dir('bin'))
    add_path('C_INCLUDE_PATH', dirs.build_dir('include'))
    add_path('LIBRARY_PATH', dirs.build_dir('lib'))
    add_path('LD_LIBRARY_PATH', dirs.build_dir('lib'))
    add_path('GPR_PROJECT_PATH', dirs.build_dir('lib', 'gnat'))
    add_path('PYTHONPATH', dirs.build_dir('python'))
    add_path('PYTHONPATH', dirs.source_dir('python_src'))


def derived_env(dirs):
    """
    Return a copy of the environment after an update using setup_environment
    """
    env = dict(os.environ)

    def add_path(name, path):
        old = env.get(name, '')
        env[name] = ('{}{}{}'.format(path, os.path.pathsep, old)
                     if old else path)

    setup_environment(dirs, add_path)
    return env


def test(args, dirs):
    """
    Run the testsuite.

    This is a wrapper around testsuite/testsuite.py tuned for interactive use:
    it correctly setups the environment according to the build directory, it
    enables colored output and it displays test outputs on error.
    """

    # Make builds available from testcases
    env = derived_env(dirs)
    argv = [
        'python',
        dirs.source_dir('testsuite', 'testsuite.py'),
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


def setenv(args, dirs):
    """
    Display Bourne shell commands that setup environment in order to make
    libadalang available.
    """
    def add_path(name, path):
        print('{name}={path}:${name}; export {name}'.format(
            name=name, path=pipes.quote(path)
        ))
    setup_environment(dirs, add_path)


def perf_test(args, dirs):
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
        args.build_mode = 'prod'
        fileutils.mkdir(args.build_dir)
        make(args, dirs)

    # Checkout the code bases that we will use for the perf testsuite
    os.chdir(work_dir)
    if not os.path.exists('gnat'):
        subprocess.check_call(['svn', 'co',
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
        fileutils.find(work_dir, '*.ads') + fileutils.find(work_dir, '*.adb')
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
        subprocess.check_call(['build/bin/parse', '-s', '-F', file_list_name])
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


def do_help(args, dirs):
    """Print usage and exit"""
    args_parser.print_help()


args_parser = argparse.ArgumentParser(
    description='General manager to handle actions relative to'
                ' building/testing libadalang'
)
subparsers = args_parser.add_subparsers()

args_parser.add_argument(
    '--build-dir', default='build',
    help=(
        'Directory to use for generated source code and binaries. By default,'
        ' use "build" in the current directory.'
    )
)
args_parser.add_argument(
    '--enable-static', action='store_true',
    help='Enabled the generation of static libraries'
)
args_parser.add_argument(
    '--disable-shared', action='store_true',
    help='Disable the generation (and testing) of shared libraries'
)
args_parser.add_argument(
    '--bindings', '-b', nargs='+', choices=('python', ),
    default=['python'],
    help='Bindings to generate (by default: only Python)'
)
args_parser.add_argument(
    '--verbose', '-v', action='store_true',
    help='Show verbose output'
)

########
# Help #
########

help_parser = subparsers.add_parser('help', help=do_help.__doc__)
help_parser.set_defaults(func=do_help)

############
# Generate #
############

generate_parser = subparsers.add_parser(
    'generate', help=generate.__doc__
)
generate_parser.add_argument(
    '--coverage', '-C', action='store_true',
    help='Compute code coverage for the code generator'
)
generate_parser.add_argument(
    '--pretty-print', '-p', action='store_true',
    help='Pretty-print generated source code'
)
generate_parser.set_defaults(func=generate)

#########
# Build #
#########

build_parser = subparsers.add_parser(
    'build', help=build.__doc__
)
build_parser.add_argument(
    '--jobs', '-j', type=int, default=get_cpu_count(),
    help='Number of parallel jobs to spawn in parallel '
         '(default: your number of cpu)'
)
build_parser.add_argument(
    '--build-mode', '-b', choices=list(BUILD_MODES), default='dev',
    help='Selects a preset for build options'
)
build_parser.add_argument(
    '--cargs', nargs='*', default=[],
    help='Options to pass as "-cargs" to GPRbuild'
)
build_parser.set_defaults(func=build)

########
# Make #
########

make_parser = subparsers.add_parser(
    'make', help=make.__doc__
)
make_parser.add_argument(
    '--jobs', '-j', type=int, default=get_cpu_count(),
    help='Number of parallel jobs to spawn in parallel'
         ' (default: your number of cpu)'
)
make_parser.add_argument(
    '--build-mode', '-b', choices=list(BUILD_MODES), default='dev',
    help='Selects a preset for build options'
)
make_parser.set_defaults(func=make)

###########
# Install #
###########

install_parser = subparsers.add_parser(
    'install', help=install.__doc__
)
install_parser.add_argument(
    'install-dir',
    help='Installation directory.'
)
install_parser.set_defaults(func=install)

########
# Test #
########

test_parser = subparsers.add_parser(
    'test', help=test.__doc__
)
test_parser.add_argument(
    'testsuite-args', nargs='*',
    help='Arguments to pass to testsuite.py.'
)
test_parser.set_defaults(func=test)

#############
# Perf Test #
#############

perf_test_parser = subparsers.add_parser(
    'perf-test', help=perf_test.__doc__
)
perf_test_parser.add_argument(
    '--jobs', '-j', type=int, default=get_cpu_count(),
    help='Number of parallel jobs to spawn in parallel'
         ' (default: your number of cpu)'
)
perf_test_parser.add_argument(
    '--work-dir', '-w', default='performance_testsuite',
    help='Directory into which the performance testsuite will be executed'
)
perf_test_parser.add_argument(
    '--nb-runs', type=int, default=4,
    help='Number of runs (default: 4)'
)
perf_test_parser.add_argument(
    '--no-recompile', action='store_true',
    help='Do not recompile the library before running the perf testsuite'
)
perf_test_parser.set_defaults(func=perf_test)


##########
# Setenv #
##########

setenv_parser = subparsers.add_parser(
    'setenv', help=setenv.__doc__
)
setenv_parser.set_defaults(func=setenv)

########
# Main #
########

if __name__ == '__main__':
    args = args_parser.parse_args()
    dirs = Directories(args)

    if args.func == generate and args.coverage:
        c = Coverage(dirs)
        with c:
            args.func(args, dirs)
        c.generate_report()
    else:
        args.func(args, dirs)
