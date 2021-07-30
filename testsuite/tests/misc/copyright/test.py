"""Check that all generated sources have the copyright notice."""

import subprocess

import libadalang

from utils import GPR_ARGS


copyright_chunk = "Libadalang is free software"


def check_file(filename):
    with open(filename) as f:
        if copyright_chunk not in f.read():
            print("Copyright notice not found in {}".format(filename))


# Check copyright notices in Ada source files
for filename in subprocess.check_output(
    ["gprls", "-Plibadalang", "-s"] + GPR_ARGS
).decode("utf-8").splitlines():
    check_file(filename)


# Check copyright notice in the Python binding
check_file(libadalang.__file__)


print("Done.")
