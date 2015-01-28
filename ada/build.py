import argparse
import env
from compile_context import CompileCtx
from glob import glob
import os
import os.path as path
import shutil
from utils import file_path

args_parser = argparse.ArgumentParser(
    description='Generate native code for libadalang'
)
args_parser.add_argument(
    '--coverage', '-c', action='store_true',
    help='Compute code coverage for the code generator'
)


class Coverage(object):
    """
    Guard object used to compute code coverage (optionally).
    """

    ROOT_DIR = path.dirname(path.dirname(path.abspath(__file__)))

    def __init__(self):
        import coverage
        self.cov = coverage.coverage(
            branch=True,
            source=[
                self.ROOT_DIR,
                path.join(self.ROOT_DIR, 'ada')
            ],
            omit=[
                path.join(self.ROOT_DIR, 'compiler', 'build.py'),
                path.join(self.ROOT_DIR, 'compiler', 'env.py'),
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
        self.cov.html_report(directory='coverage', ignore_errors=True)


def main():
    context = CompileCtx("compilation_unit")
    context.set_lexer_file(path.join(file_path(__file__), "ada.qx"))

    from ada_parser import A
    import ada_parser.decl
    import ada_parser.types
    import ada_parser.exprs
    import ada_parser.bodies

    context.set_grammar(A)
    context.emit(file_root="build", file_name="parse")


if __name__ == '__main__':
    args = args_parser.parse_args()
    if args.coverage:
        c = Coverage()
        with c:
            main()
        c.generate_report()
    else:
        main()
