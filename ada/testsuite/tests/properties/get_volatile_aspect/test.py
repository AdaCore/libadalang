from __future__ import absolute_import, division, print_function

import libadalang as lal


ctx = lal.AnalysisContext()
unit = ctx.get_from_file("test.adb")

assert not unit.diagnostics

for n in unit.root.findall(lal.BasicDecl):
    print("{}:".format(n))
    print("  volatile aspect: {}".format(n.p_get_aspect("volatile")))

print("Done.")
