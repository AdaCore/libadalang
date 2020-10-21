"""
Test that one can customize the tab stop to use when lexing. Also make sure
this parameter is properly sanitized.
"""

import libadalang as lal


c = lal.AnalysisContext('utf-8', tab_stop=20)
u = c.get_from_buffer('foo.ads', b'\tprocedure Foo;\n')
print(u.root.sloc_range)


try:
    lal.AnalysisContext('utf-8', tab_stop=0)
except ValueError as exc:
    print('ValueError: {}'.format(exc))

print('Done')
