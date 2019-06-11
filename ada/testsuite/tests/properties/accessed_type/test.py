from __future__ import absolute_import, division, print_function

import sys

import libadalang as lal


for filename in sys.argv[1:]:
    print('== {} =='.format(filename))
    u = lal.AnalysisContext().get_from_file(filename)
    assert not u.diagnostics

    for td in u.root.findall(lal.BaseTypeDecl):
        print("{}:".format(td))
        print("  accessed type: {}".format(td.p_accessed_type()))

    print('')

print('Done')
