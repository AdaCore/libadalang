import sys

import libadalang as lal


for filename in sys.argv[1:]:
    print('== {} =='.format(filename))
    u = lal.AnalysisContext().get_from_file(filename)
    assert not u.diagnostics

    for n in u.root.findall(lal.ObjectDecl):
        e = n.f_default_expr
        if e.p_is_static_expr():
            print('{} is a static expr'.format(n))
        else:
            print('{} is not a static expr'.format(n))

    print('')

print('Done')
