"""
Test that AdaNode.lookup works properly.
"""

import libadalang as lal


ctx = lal.AnalysisContext()
unit = ctx.get_from_file('foo.adb')

for line, column in [(0, 0), (1, 1), (5, 80), (6, 16), (8, 12)]:
    sloc = lal.Sloc(line, column)
    n = unit.root.lookup(sloc)
    print('Lookup {}: {}'.format(sloc, n))

print('Done.')
