from __future__ import absolute_import, division, print_function

import libadalang as lal

ctx = lal.AnalysisContext()
unit = ctx.get_from_file("test.adb")

assert not unit.diagnostics

for call in unit.root.findall(lal.CallExpr):
    for name in call.findall(lal.Name):
        print("{} {} a call and {} a dot call".format(
            name.text,
            "is" if name.p_is_call else "is not",
            "is" if name.p_is_dot_call() else "is not"
        ))

print("Done.")
