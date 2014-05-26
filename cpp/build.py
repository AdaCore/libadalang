import sys
from os import path

sys.path.extend([path.abspath('../src')])
from syntax import A

import syntax.decl
import syntax.types
import syntax.exprs
import syntax.bodies

A.dump_to_file(file_path="src/", file_name="parse")
