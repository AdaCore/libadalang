"""
Test that the p_semantic_parent property works as one would expect.
"""

import libadalang as lal


c = lal.AnalysisContext('utf-8')
u = c.get_from_file("foo.adb")

ps = u.root.find(lal.ParamSpec)
print(ps.p_semantic_parent)

btd = u.root.find(lal.BaseTypeDecl)
print(btd.p_semantic_parent)
