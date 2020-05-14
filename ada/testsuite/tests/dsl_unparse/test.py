"""
Test that dsl_unparse does not crash on libadalang.
"""
from __future__ import absolute_import, division, print_function

import os
import subprocess
import sys


unparse_dest = os.path.abspath('lal.lkt')
unparse_script = 'to:{},lexer,grammar,nodes'.format(unparse_dest)

status = subprocess.call(
    [sys.executable, os.path.join('ada', 'manage.py'), '-v=none', '-E',
     'generate', '-P', '--unparse-script', unparse_script],
    cwd=os.environ['LIBADALANG_ROOTDIR']
)
if status == 0:
    try:
        with open(unparse_dest, 'r') as lkt_file:
            next(lkt_file)
        print("Successfully unparsed libadalang.")
    except IOError:
        print("{} not found, unparsing libadalang failed.".format(
            unparse_dest
        ))
    except StopIteration:
        print("{} is empty, unparsing libadalang failed.".format(
            unparse_dest
        ))
