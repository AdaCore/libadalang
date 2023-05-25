"""
Check that App reports missing source files as expected.
"""

import dataclasses
import os.path
import subprocess
import sys
from typing import List

import libadalang as lal


@dataclasses.dataclass
class Testcase:
    """
    Each testcase runs the ``App`` below and passes it the command line
    arguments ``args``.
    """
    label: str
    args: List[str]


class App(lal.App):
    def process_unit(self, unit):
        print("Processing:", os.path.basename(unit.filename))
        unit.populate_lexical_env(1)


tests = [
    Testcase("no_missing", ["no_missing.adb", "no_such_file.adb"]),
    Testcase(
        "missing_warn",
        ["-k", "no_missing.adb", "missing.adb", "no_such_file.adb"],
    ),
    Testcase(
        "missing_error",
        ["no_missing.adb", "missing.adb", "no_such_file.adb"],
    ),
]


if len(sys.argv) == 1:
    print("test.py: Starting...")
    print("")
    for t in tests:
        print(f"== {t.label} ==")
        print("")
        sys.stdout.flush()
        p = subprocess.run([sys.executable, __file__] + t.args)
        print("Return code:", p.returncode)
        print("")
    print("test.py: Done.")
else:
    App.run(sys.argv[1:])
