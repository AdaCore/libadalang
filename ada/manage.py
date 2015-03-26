#! /usr/bin/env python

import argparse
import os.path
import subprocess
import multiprocessing
import pipes

# Set the environment
from env import setenv
setenv()

from gnatpython import fileutils
from gnatpython.ex import which
import sys
from c_api import CAPISettings
from compile_context import CompileCtx
from python_api import PythonAPISettings
from utils import Colors


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


def get_default_compiler():
    """
    Return either "clang" or "gcc" depending on what is available.
    """
    if which('clang') and which('clang++'):
        return 'clang'
    elif which('gcc') and which('g++'):
        return 'gcc'
    else:
        raise RuntimeError('Could not find a C/C++ native toolchain')


def get_compilers(name):
    """
    Return a couple (C compiler, C++ compiler) corresponding to "name".

    Name can be either "gcc" or "clang".
    """
    return {
        'clang': ('clang', 'clang++'),
        'gcc':   ('gcc',   'g++'),
    }[name]


def generate(args, dirs):
    """Generate source code for libadalang."""
    lexer_file = dirs.source_dir('ada', 'ada.qx')
    c_api_settings = CAPISettings(
        'libadalang',
        symbol_prefix='ada',
    )
    python_api_settings = (PythonAPISettings('libadalang', c_api_settings)
                           if 'python' in args.bindings else None)
    context = CompileCtx('ada', lexer_file, 'compilation_unit',
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

    print Colors.HEADER + "Generating source for libadalang ..." + Colors.ENDC
    context.emit(file_root=dirs.build_dir())
    print Colors.OKGREEN + "Generation complete !" + Colors.ENDC


def build(args, dirs):
    """Build generated source code."""
    c_compiler, cxx_compiler = get_compilers(args.compiler)
    make_argv = ['make', '-C', dirs.build_dir(),
                 '-j{}'.format(args.jobs),
                 'CC={}'.format(c_compiler),
                 'CXX={}'.format(cxx_compiler)]
    make_argv.extend(getattr(args, 'make-options'))
    print (
        Colors.HEADER
        + "Building the generated source code ..." + Colors.ENDC
    )
    try:
        subprocess.check_call(make_argv)
    except subprocess.CalledProcessError as exc:
        print >> sys.stderr, 'Build failed: {}'.format(exc)
        sys.exit(1)
    print Colors.OKGREEN + "Compilation complete !" + Colors.ENDC


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
    add_path('PYTHONPATH', dirs.build_dir('python'))


def test(args, dirs):
    """
    Run the testsuite.

    This is a wrapper around testsuite/testsuite.py tuned for interactive use:
    it correctly setups the environment according to the build directory, it
    enables colored output and it displays test outputs on error.
    """

    # Make builds available from testcases
    env = dict(os.environ)

    def add_path(name, path):
        old = env.get(name, '')
        env[name] = '{}:{}'.format(path, old) if old else path
    setup_environment(dirs, add_path)

    try:
        subprocess.check_call([
            dirs.source_dir('testsuite', 'testsuite.py'),
            '--enable-color', '--show-error-output',
        ] + getattr(args, 'testsuite-args'), env=env)
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


def do_help(args, dirs):
    """Print usage and exit"""
    args_parser.print_help()


args_parser = argparse.ArgumentParser(
    description='Generate native code for libadalang'
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
    '--compiler', '-c', choices=('clang', 'gcc'),
    default=get_default_compiler(),
    help='Select what native toolchain to use (Clang or GCC)'
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

# Help
help_parser = subparsers.add_parser('help', help=do_help.__doc__)
help_parser.set_defaults(func=do_help)

# Generate
generate_parser = subparsers.add_parser(
    'generate', help=generate.__doc__
)
generate_parser.add_argument(
    '--coverage', '-C', action='store_true',
    help='Compute code coverage for the code generator'
)
generate_parser.set_defaults(func=generate)

# Build
build_parser = subparsers.add_parser(
    'build', help=build.__doc__
)
build_parser.add_argument(
    '--jobs', '-j', type=int, default=multiprocessing.cpu_count(),
    help='Number of parallel jobs to spawn in parallel '
         '(default: your number of cpu)'
)
build_parser.add_argument(
    'make-options', nargs='*',
    help='Options to pass directly to make'
)
build_parser.set_defaults(func=build)

# Make
make_parser = subparsers.add_parser(
    'make', help=make.__doc__
)
make_parser.add_argument(
    '--jobs', '-j', type=int, default=multiprocessing.cpu_count(),
    help='Number of parallel jobs to spawn in parallel'
         ' (default: your number of cpu)'
)
make_parser.add_argument(
    'make-options', nargs='*',
    help='Options to pass directly to make'
)
make_parser.set_defaults(func=make)

# Install
install_parser = subparsers.add_parser(
    'install', help=install.__doc__
)
install_parser.add_argument(
    'install-dir',
    help='Installation directory.'
)
install_parser.set_defaults(func=install)

# Test
test_parser = subparsers.add_parser(
    'test', help=test.__doc__
)
test_parser.add_argument(
    'testsuite-args', nargs='*',
    help='Arguments to pass to testsuite.py.'
)
test_parser.set_defaults(func=test)

# Setenv
setenv_parser = subparsers.add_parser(
    'setenv', help=setenv.__doc__
)
setenv_parser.set_defaults(func=setenv)


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
