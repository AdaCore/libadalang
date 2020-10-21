import sys

import libadalang as lal


ctx = lal.AnalysisContext()
u = ctx.get_from_file('pkg.ads')
if u.diagnostics:
    for d in u.diagnostics:
        print('error: {}'.format(d))
    sys.exit(1)

decls = u.root.findall(lal.ObjectDecl)
d1, d2 = decls
n = d2.f_default_expr

node_dict = dict((node, repr(node)) for node in (d1, d2, n))

print('d1: {}'.format(d1))
print('d2: {}'.format(d2))
if not d2.p_resolve_names:
    print('Resolution failed')
    sys.exit(1)

resolved = n.p_referenced_decl()
print('resolved: {}'.format(resolved))

if d1 != d1:
    print('Self comparison failed')

new_d1 = u.root.find(lal.ObjectDecl)
if d1 != new_d1:
    print('Simple comparison failed')

if d1 == resolved:
    print('Entity info ignored for comparison')

print('Dict lookup: {}'.format(node_dict[new_d1]))

print('Done.')
