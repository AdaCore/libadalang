"""
Check that ``FileReader.create_preprocessor_from_file`` works as expected.
"""

from typing import List, Optional

import libadalang as lal


def check(label: str,
          path: List[str],
          line_mode: Optional[lal.FileReader.LineMode] = None) -> None:
    print(f"== {label} ==")
    print("")

    try:
        fr = lal.FileReader.create_preprocessor_from_file(
            "prep.txt", path, line_mode
        )
    except (lal.FileReadError, lal.SyntaxError) as exc:
        print("Error while parsing preprocessor data file:")
        print(f"  {type(exc).__name__}: {exc}")
        print("")
        return

    ctx = lal.AnalysisContext(file_reader=fr)
    u = ctx.get_from_file("foo.adb")
    print("Preprocessor output on foo.adb:")
    for i, line in enumerate(u.text.split("\n"), 1):
        print(f"  {str(i).rjust(2)} | {line}".rstrip())
    print("")


check("Missing file", ["."])
check("Syntax error", ["d3"])
check("Normal (default line mode)", ["d1", "d2"])
check("Normal (comment line mode)", ["d1", "d2"],
      lal.FileReader.LineMode.comment_lines)

print('Done')
