"""
Check that ``GPRProject.create_preprocessor`` works as expected.
"""

from typing import List, Optional

import libadalang as lal


def check(label: str, project: str) -> None:
    print(f"== {label} ==")
    print("")

    prj = lal.GPRProject(project)

    try:
        fr = prj.create_preprocessor()
    except (lal.FileReadError, lal.SyntaxError) as exc:
        print("Error while creating the preprocessor:")
        print(f"  {type(exc).__name__}: {exc}")
        print("")
        return

    ctx = lal.AnalysisContext(file_reader=fr)
    u = ctx.get_from_file("src/foo.adb")
    print("Preprocessor output on foo.adb:")
    for i, line in enumerate(u.text.split("\n"), 1):
        print(f"  {str(i).rjust(2)} | {line}".rstrip())
    print("")


check("Normal", "normal.gpr")
check("No such file", "no_such_file.gpr")
check("Syntax error", "syntax_error.gpr")

print('Done')
