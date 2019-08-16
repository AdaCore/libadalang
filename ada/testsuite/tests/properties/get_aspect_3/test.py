from __future__ import absolute_import, division, print_function

import libadalang as lal
import sys


for f in sys.argv[1:]:
    u = lal.AnalysisContext().get_from_file(f)
    assert not u.diagnostics

    n = u.root.findall(lal.BaseTypeDecl)[1]
    print("{} get_aspect('pack') -> {}".format(
        n, n.p_get_aspect('pack')
    ))
    print('')
    print('Done')
