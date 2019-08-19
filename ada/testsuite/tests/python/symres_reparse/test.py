"""
Test that lexical environments still work when resolving from a parent package,
when the parent package has been reparsed.
"""

from __future__ import absolute_import, division, print_function

import libadalang as lal


c = lal.AnalysisContext('utf-8')
u = c.get_from_file("foo-bar.ads")
i = u.root.find(lal.PragmaArgumentAssoc).f_expr
print(i.p_matching_nodes[0])

u2 = c.get_from_file("foo.ads", reparse=True)

print(i.p_matching_nodes[0])
