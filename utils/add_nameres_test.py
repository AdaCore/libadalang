#! /usr/bin/env python
"""
This script is a helper to add a test for symbol resolution. It takes as
being the entry point for name resolution. It will run the name resolution
driver on the files and create a test from it.
"""

import argparse
from os import path
import os
import shutil
import subprocess


parser = argparse.ArgumentParser(
    description=__doc__
)
parser.add_argument("test_name", help="The name of the test")
parser.add_argument(
    "ada_files", nargs='+',
    help="The files for the test. The first file is the main one"
)
parser.add_argument('--with-default-project', action='store_true')
args = parser.parse_args()

test_name = args.test_name
ada_file_paths = map(os.path.abspath, args.ada_files)
file_names = map(path.basename, ada_file_paths)
main_file_name = file_names[0]

yaml = """driver: name-resolution
input_sources: [{main_file_name}]
with_default_project: {args.with_default_project}
""".format(**locals())

os.chdir('ada/testsuite/tests/name_resolution')
if not os.path.isdir(test_name):
    os.makedirs(test_name)

for ada_file_path, file_name in zip(ada_file_paths, file_names):
    shutil.copy(ada_file_path, test_name)

os.chdir(test_name)
print(os.getcwd())
with open("test.out", "w") as test_out:
    try:
        output = subprocess.check_output(
            ["nameres"]
            + (["--with-default-project"]
               if args.with_default_project else [])
            + [main_file_name]
        )
        print("Name res succeeded, result:\n\n{}".format(output))
        test_out.write(output)
    except subprocess.CalledProcessError as e:
        print("Error calling name res: {}".format(e.output))

with open(path.join("test.yaml"), 'w') as f:
    f.write(yaml)
