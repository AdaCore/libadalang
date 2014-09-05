import argparse
import env
from glob import glob
import os
import os.path as path
import shutil
import subprocess
import sys

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

    def __init__(self, enable=True):
        self.enabled = enable
        if self.enabled:
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
        if self.enabled:
            self.cov.start()

    def __exit__(self, value, type, traceback):
        if self.enabled:
            self.cov.stop()

    def generate_report(self):
        if self.enabled:
            self.cov.html_report(directory='coverage', ignore_errors=True)


def main(args):
    c = Coverage(args.coverage)
    with c:
        for d in [
            "build", "build/include", "build/obj", "build/src", "build/bin"
        ]:
            if not path.exists(d):
                os.mkdir(d)

        from ada_parser import A
        import ada_parser.decl
        import ada_parser.types
        import ada_parser.exprs
        import ada_parser.bodies

        A.dump_to_file(file_path="build/src/", file_name="parse")
        shutil.copy("support/Makefile", "build/Makefile")

        for f in glob("support/*.hpp"):
            shutil.copy(f, "build/include")

        for f in glob("support/*.cpp"):
            shutil.copy(f, "build/src")

        subprocess.check_call(["quex", "-i", "../../ada/ada.qx",
                               "--engine", "quex_lexer",
                               "--token-id-offset",  "0x1000",
                               "--language", "C",
                               "--no-mode-transition-check",
                               "--single-mode-analyzer",
                               "--token-memory-management-by-user",
                               "--token-policy", "single"], cwd="build/src")
    c.generate_report()


if __name__ == '__main__':
    args = args_parser.parse_args()
    main(args)
