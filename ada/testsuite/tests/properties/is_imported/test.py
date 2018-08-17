from __future__ import absolute_import, division, print_function

import sys

import libadalang as lal


for filename in sys.argv[1:]:
    print('== {} =='.format(filename))
    u = lal.AnalysisContext().get_from_file(filename)
    assert not u.diagnostics

    for n in u.root.findall(lambda _: True):
        if hasattr(n, 'p_is_imported'):
            print('{} -> {}'.format(n, n.p_is_imported))

    print('')

print('Done')
