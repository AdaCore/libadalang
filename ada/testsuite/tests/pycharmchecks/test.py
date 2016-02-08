from distutils.spawn import find_executable
from os import path, environ
from subprocess import check_call
import sys

try:
    from psutil import process_iter
except ImportError:
    pycharm = []
else:
    pycharm = [p for p in process_iter() if p.name().find("pycharm") != -1]

if find_executable('pycharm.sh'):
    if not pycharm:
        check_call([
            sys.executable,
            path.join(
                environ['LIBADALANG_ROOTDIR'], "..",
                "utils",
                "print_pycharm_diagnostics.py"
            )
        ])
