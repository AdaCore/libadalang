from __future__ import absolute_import, division, print_function

from libadalang import AnalysisContext


ctx = AnalysisContext('iso-8859-1')
unit = ctx.get_from_file('foo.adb')
unit.root.dump()
print('Done')
