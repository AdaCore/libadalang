from distutils.spawn import find_executable
from os import path, environ
import sys
from subprocess import check_call

if find_executable('pycharm.sh'):
    check_call([
        sys.executable,
        path.join(
            environ['LIBADALANG_ROOTDIR'], "..",
            "utils",
            "print_pycharm_diagnostics.py"
        )
    ])
