import sys

import libadalang as lal


for filename in sys.argv[1:]:
    print('== {} =='.format(filename))
    u = lal.AnalysisContext().get_from_file(filename)
    assert not u.diagnostics

    for t in u.root.findall(lal.TypeDecl):
        for prim in t.p_get_primitives():
            if t.p_is_inherited_primitive(prim):
                print('For {}, {} is inherited'.format(t, prim))
            else:
                print('For {}, {} is not inherited'.format(t, prim))

    print('')

print('Done')
