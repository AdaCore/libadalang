"""
Test that the p_unique_identifying_name property works as one would expect.
"""

from __future__ import absolute_import, division, print_function

import libadalang as lal


c = lal.AnalysisContext('utf-8')

for f in ["test.adb", "test.ads"]:
    u = c.get_from_file(f)

    for s in u.root.findall(
        lambda n: n.is_a(lal.BasicDecl) and n.p_is_subprogram
    ):
        try:
            print("{} uuid is {}".format(s, s.p_unique_identifying_name))
        except lal.PropertyError:
            print("Error on uuid for {}".format(s))
            raise
