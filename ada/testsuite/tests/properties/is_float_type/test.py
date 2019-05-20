from __future__ import absolute_import, division, print_function

import sys

import libadalang as lal


for filename in sys.argv[1:]:
    print('== {} =='.format(filename))
    u = lal.AnalysisContext().get_from_file(filename)
    assert not u.diagnostics

    for tdecl in u.root.findall(lal.BaseTypeDecl):
        print("{} {} a floating point type.".format(
            tdecl, "is" if tdecl.p_is_float_type else "is not"
        ))

    print('')

print('Done')
