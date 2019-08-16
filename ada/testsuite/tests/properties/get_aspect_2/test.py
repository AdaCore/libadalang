from __future__ import absolute_import, division, print_function

import sys

import libadalang as lal


for filename in sys.argv[1:]:
    print('== {} =='.format(filename))
    u = lal.AnalysisContext().get_from_file(filename)
    assert not u.diagnostics

    for n in u.root.findall(lal.PackageDecl):
        print("{} get_aspect('preelaborate') -> {}".format(
            n,
            n.p_get_aspect('preelaborate'),
        ))

    print('')

print('Done')
