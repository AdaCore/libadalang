from __future__ import absolute_import, division, print_function

import libadalang as lal

ctx = lal.AnalysisContext()
unit = ctx.get_from_file("imp.ads")

assert not unit.diagnostics

ittd = unit.root.find(lal.IncompleteTypeDecl)
print(ittd.p_next_part_for_decl)
