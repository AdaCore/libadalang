from __future__ import absolute_import, division, print_function

import libadalang as lal


ctx = lal.AnalysisContext()
u = ctx.get_from_file("test.adb")

assert not u.diagnostics

types = u.root.findall(
    lambda n: n.is_a(lal.TypeDecl)
)

for tpe in types:
    print("Types deriving from {}:".format(tpe.p_fully_qualified_name))
    for t in tpe.p_find_all_derived_types([u]):
        print("  {}".format(t.p_fully_qualified_name))

print('Done')
