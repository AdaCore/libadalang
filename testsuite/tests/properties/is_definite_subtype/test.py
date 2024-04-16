import sys

import libadalang as lal


for filename in sys.argv[1:]:
    print('== {} =='.format(filename))
    u = lal.AnalysisContext().get_from_file(filename)
    assert not u.diagnostics

    for n in u.root.findall(lal.BaseTypeDecl):
        if n.p_is_definite_subtype():
            print('{} is a definite subtype'.format(n))
        else:
            print('{} is not a definite subtype'.format(n))

    for n in u.root.findall(lal.ObjectDecl):
        te = n.f_type_expr
        if te.p_is_definite_subtype:
            print('{} is a definite subtype'.format(te))
        else:
            print('{} is not a definite subtype'.format(te))

    print('')

print('Done')
