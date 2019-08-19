from __future__ import absolute_import, division, print_function

import sys

import libadalang as lal


for filename in sys.argv[1:]:
    print('== {} =='.format(filename))
    u = lal.AnalysisContext().get_from_file(filename)
    assert not u.diagnostics

    for bd in u.root.findall(lal.BasicDecl):
        try:
            n = bd.p_fully_qualified_name
        except lal.PropertyError:
            n = '<error>'
        print('{} -> {}'.format(bd, n))

    print('')

print('Done')
