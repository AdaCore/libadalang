"""
Check that ``GPRProject.source_files`` works as expected.
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


prjs = {
    "orig": lal.GPRProject("root.gpr"),
    "alt": lal.GPRProject("root.gpr", scenario_vars={"LIB_ALT": "true"}),
}

for prj, kwargs in [
    ("orig", {}),
    ("orig", {"mode": lal.SourceFilesMode.default}),
    ("orig", {"mode": lal.SourceFilesMode.root_project}),
    ("orig", {"mode": lal.SourceFilesMode.whole_project}),
    ("orig", {"mode": lal.SourceFilesMode.root_project, "projects": ["lib"]}),
    ("orig", {"mode": lal.SourceFilesMode.root_project,
              "projects": ["lib", "ext"]}),
    ("alt", {}),
]:
    print(f"{prj} {kwargs}:")
    sf = prjs[prj].source_files(**kwargs)
    for f in sf:
        print(f"  {inside_test_dir(f) or f}")
    print("")


print("invalid project:")
try:
    lal.GPRProject("foo.gpr")
except lal.InvalidProject:
    print("  <InvalidProject exception>")
else:
    print("  Unexpected absence of exception")
print("")


print("invalid sub-project:")
try:
    prjs[prj].source_files(projects=["nosuchproject"])
except lal.InvalidProject:
    print("  <InvalidProject exception>")
else:
    print("  Unexpected absence of exception")
print("")


print("with runtime:")
prj = lal.GPRProject("root.gpr")
files = prj.source_files(lal.SourceFilesMode.whole_project_with_runtime)
if "system.ads" in {os.path.basename(f) for f in files}:
    print("  system.ads is present")
    for f in files:
        rel_f = inside_test_dir(f)
        if rel_f:
            print(f"  {rel_f}")
print("")

print('Done')
