from __future__ import absolute_import, division, print_function

import libadalang


ctx = libadalang.AnalysisContext('iso-8859-1')
unit = ctx.get_from_file('foo.adb')
print('Diagnostics for foo.adb:')
for diag in unit.diagnostics:
    print('  {}'.format(diag))
print('Done.')
