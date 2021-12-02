"""
Check that ``SourceFiles.for_project`` works as expected.
"""

from dataclasses import dataclass
import os.path

import libadalang as lal


cwd = os.path.realpath(os.getcwd())


def inside_test_dir(f):
    """
    If ``f`` is a filename under this test directory, return it relative to it.
    Return None otherwise. Also canonicalize paths to Unix-like syntax.
    """
    f = os.path.realpath(f)
    if not f.startswith(cwd):
        return None
    return os.path.relpath(f, cwd).replace("\\", "/")


for kwargs in [
    {},
    {"mode": lal.SourceFilesMode.default},
    {"mode": lal.SourceFilesMode.root_project},
    {"mode": lal.SourceFilesMode.whole_project},
    {"scenario_vars": {"LIB_ALT": "true"}},
]:
    print(f"{kwargs}:")
    for f in lal.SourceFiles.for_project("root.gpr", **kwargs):
        print(f"  {inside_test_dir(f) or f}")
    print("")


print("invalid project:")
try:
    lal.SourceFiles.for_project("foo.gpr")
except lal.InvalidProject:
    print("<InvalidProject exception>")
else:
    print("Unexpected absence of exception")
print("")


print("with runtime:")
files = lal.SourceFiles.for_project(
    "root.gpr", mode=lal.SourceFilesMode.whole_project_with_runtime
)
if "system.ads" in {os.path.basename(f) for f in files}:
    print("  system.ads is present")
    for f in files:
        rel_f = inside_test_dir(f)
        if rel_f:
            print(f"  {rel_f}")


print('Done')
