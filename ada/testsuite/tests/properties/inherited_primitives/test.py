from __future__ import absolute_import, division, print_function

import libadalang as lal


ctx = lal.AnalysisContext()
u = ctx.get_from_file("test.adb")

assert not u.diagnostics

types = u.root.findall(
    lambda n: n.is_a(lal.TypeDecl) and n.p_defining_name.text == "U"
)

for tpe in types:
    print("Primitives inherited by {}:".format(tpe))
    for p in tpe.p_get_primitives(False):
        print("  {}".format(p))

print('Done')
