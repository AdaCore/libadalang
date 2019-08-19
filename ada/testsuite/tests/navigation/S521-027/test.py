from __future__ import absolute_import, division, print_function

import libadalang as lal


ctx = lal.AnalysisContext()
unit = ctx.get_from_file("test.adb")

assert not unit.diagnostics

exc_decl = unit.root.find(lal.ExceptionDecl)
dn = exc_decl.f_ids[0]

print("{}.p_xref = {}".format(
    dn,
    dn.p_xref()
))

print("Done.")
