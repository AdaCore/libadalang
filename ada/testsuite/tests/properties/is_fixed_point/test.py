import sys

import libadalang as lal


for filename in sys.argv[1:]:
    print('== {} =='.format(filename))
    u = lal.AnalysisContext().get_from_file(filename)
    assert not u.diagnostics

    for tdecl in u.root.findall(lal.BaseTypeDecl):
        print("{} {} a fixed point type.".format(
            tdecl, "is" if tdecl.p_is_fixed_point() else "is not"
        ))

    print('')

print('Done')
