#! /usr/bin/env python

import argparse
from glob import glob
import os.path
import shutil
import subprocess

from gnatpython import fileutils
import env
from compile_context import CompileCtx
from utils import file_path


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
    def source_dir(self):
        return self.root_dir

    @property
    def build_dir(self):
        return os.path.abspath(self.args.build_dir)

    @property
    def install_dir(self):
        return os.path.abspath(getattr(self.args, 'install-dir'))

    def under_source(self, *args):
        return os.path.join(self.source_dir, *args)

    def under_build(self, *args):
        return os.path.join(self.build_dir, *args)

    def under_install(self, *args):
        return os.path.join(self.install_dir, *args)


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
                self.dirs.source_dir,
                self.dirs.under_source('ada'),
            ],
            omit=[
                self.dirs.under_source('compiler', 'build.py'),
                self.dirs.under_source('compiler', 'env.py'),
            ],
        )

        self.cov.exclude('def __repr__')
        self.cov.exclude('raise NotImplementedError()')
        self.cov.exclude('assert False')

    def __enter__(self):
        self.cov.start()

    def __exit__(self, value, type, traceback):
        self.cov.stop()

    def generate_report(self):
        self.cov.html_report(
            directory=self.dirs.under_build('coverage'),
            ignore_errors=True
        )


def generate(args, dirs):
    """Generate source code for libadalang."""
    context = CompileCtx('ada', 'compilation_unit')
    context.set_lexer_file(dirs.under_source('ada', 'ada.qx'))

    from ada_parser import A
    import ada_parser.decl
    import ada_parser.types
    import ada_parser.exprs
    import ada_parser.bodies

    context.set_grammar(A)
    context.emit(file_root=dirs.build_dir, file_name='parse')


def build(args, dirs):
    """Build generated source code."""
    try:
        subprocess.check_call([
            'make', '-C', dirs.build_dir, '-j{}'.format(args.jobs),
        ] + list(getattr(args, 'make-options')))
    except subprocess.CalledProcessError as exc:
        print >> sys.stderr, 'Build failed: {}'.format(exc)
        sys.exit(1)


def install(args, dirs):
    """Install programs and libraries."""
    for subdir in ('bin', 'include', 'lib'):
        fileutils.sync_tree(
            dirs.under_build(subdir),
            dirs.under_install(subdir)
        )


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

generate_parser = subparsers.add_parser(
    'generate', help=generate.__doc__
)
generate_parser.add_argument(
    '--coverage', '-c', action='store_true',
    help='Compute code coverage for the code generator'
)
generate_parser.set_defaults(func=generate)

build_parser = subparsers.add_parser(
    'build', help=build.__doc__
)
build_parser.add_argument(
    '--jobs', '-j', type=int, default=1,
    help='Number of parallel jobs to spawn in parallel (default: only one)'
)
build_parser.add_argument(
    'make-options', nargs='*',
    help='Options to pass directly to make'
)
build_parser.set_defaults(func=build)

install_parser = subparsers.add_parser(
    'install', help=install.__doc__
)
install_parser.add_argument(
    'install-dir',
    help='Installation directory.'
)
install_parser.set_defaults(func=install)


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
