from distutils.spawn import find_executable
from os import path, environ
from psutil import process_iter
from subprocess import check_call
import sys

if find_executable('pycharm.sh'):
    pycharm = [p for p in process_iter() if p.name().find("pycharm") != -1]
    if not pycharm:
        check_call([
            sys.executable,
            path.join(
                environ['LIBADALANG_ROOTDIR'], "..",
                "utils",
                "print_pycharm_diagnostics.py"
            )
        ])
