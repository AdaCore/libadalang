"""
Test that dsl_unparse does not crash on libadalang.
"""
import os
import subprocess
import sys


unparse_dest = os.path.abspath('lal.lkt')
unparse_script = 'to:{},lexer,grammar,nodes'.format(unparse_dest)

status = subprocess.call(
    [sys.executable, os.path.join('ada', 'manage.py'),
     '-v=none', '-E',

     # The call to "generate" will generate not only the concrete DSL, but also
     # the Ada sources. Target a build directory that is local to this
     # testcase's working directory to avoid messing with the sources of the
     # library we are currently testing, as this could trigger parallel
     # compilations of Libadalang, leading to obscure failures.
     '--build-dir', os.path.abspath('build'),

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
