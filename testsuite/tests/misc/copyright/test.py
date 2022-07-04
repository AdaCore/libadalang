"""Check that all generated sources have the copyright notice."""

import os
import subprocess

import libadalang

from utils import GPR_ARGS


copyright_chunk = "SPDX-License-Identifier: Apache-2.0"


def check_file(filename):
    with open(filename) as f:
        if copyright_chunk not in f.read():
            print("Copyright notice not found in {}".format(filename))


print("Checking Ada source files")
for filename in subprocess.check_output(
    ["gprls", "-Plibadalang", "-s"] + GPR_ARGS
).decode("utf-8").splitlines():
    check_file(filename)

print("Checking Python bindings")
lal_py_src = libadalang.__file__
if lal_py_src.endswith(".pyc"):
    lal_py_src = lal_py_src[:-1]
check_file(lal_py_src)

print("Checking OCaml bindings (if present)")
ocaml_bindings = os.environ.get("LAL_OCAML_BINDINGS")
if ocaml_bindings:
    for filename in ("libadalang.ml", "libadalang.mli"):
        check_file(os.path.join(ocaml_bindings, filename))

print("Done.")
