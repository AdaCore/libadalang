from __future__ import absolute_import, division, print_function

import sys

import libadalang as lal


ctx = lal.AnalysisContext()
u = ctx.get_from_file('pkg.ads')
if u.diagnostics:
    for d in u.diagnostics:
        print('error: {}'.format(d))
    sys.exit(1)
u.populate_lexical_env()

decls = u.root.findall(lal.ObjectDecl)
d1, d2 = decls
n = d2.f_default_expr

print('d1:', d1)
print('d2:', d2)
if not d2.p_resolve_names:
    print('Resolution failed')
    sys.exit(1)

resolved = n.p_ref_val
print('resolved:', resolved)

if d1 != d1:
    print('Self comparison failed')

if d1 != u.root.find(lal.ObjectDecl):
    print('Simple comparison failed')

if d1 == resolved:
    print('Entity info ignored')

print('Done.')
