from __future__ import absolute_import, division, print_function

import libadalang as lal


ctx = lal.AnalysisContext()
u = ctx.get_from_file("test.adb")
multi = ctx.get_from_file("multi_units.adb")

assert not u.diagnostics and not multi.diagnostics

d = u.root.find(lal.DefiningName)
print(d.p_find_all_references([multi]))

print('Done')
