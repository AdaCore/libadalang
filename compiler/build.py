import sys
from os import path
import os
import shutil
import subprocess
from glob import glob

current_dir = path.dirname(path.abspath(__file__))
sys.path.extend([path.join(current_dir, '../ada')])

for d in ["build", "build/include", "build/obj", "build/src", "build/bin"]:
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
