import sys

import libadalang as lal


for filename in sys.argv[1:]:
    print('== {} =='.format(filename))
    u = lal.AnalysisContext().get_from_file(filename)
    assert not u.diagnostics

    for n in u.root.findall(lal.BaseTypeDecl):
        if n.p_is_static_decl():
            print('{} is a static decl'.format(n))
        else:
            print('{} is not a static decl'.format(n))

print('Done')
