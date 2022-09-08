import sys

import libadalang as lal


ctx = lal.AnalysisContext()
u = ctx.get_from_file('pkg.ads')

d1, d2 = u.root.findall(lal.TypeDecl)

prim_1 = d1.p_get_primitives()[0]
prim_2 = d2.p_get_primitives()[0]

e = u.root.findall(lal.BaseSubpBody)[1]

prim_3 = e.f_expr[0].p_referenced_decl()

print(f'Prim_T.Primitive = Der_T.Primitive: {prim_1 == prim_2}')
print('Der_T.Primitive = Der_T.Primitive '
      f'(different internal metadata): {prim_2 == prim_3}')

print('Done.')
