from __future__ import absolute_import, division, print_function

import libadalang as lal
from libadalang import _py2to3


ctx = lal.AnalysisContext()
unit = ctx.get_from_file('test.adb')
unit2 = ctx.get_from_file('test2.adb')

print('First and last tokens for test.adb:')
print('  * {}'.format(unit.first_token))
print('  * {}'.format(unit.last_token))
print('')

print('Whole source buffer for test.adb:')
print(_py2to3.text_repr(lal.Token.text_range(unit.first_token,
                                             unit.last_token)))
print('')

last = unit.first_token.next
first = unit.last_token.previous
print('Empty range for the following bounds:')
print('  * {}'.format(first))
print('  * {}'.format(last))
print(_py2to3.text_repr(lal.Token.text_range(first, last)))
print('')

print('Source excerpts for all Basic_Decl in test.adb:')
for n in unit.root.findall(lal.BasicDecl):
    print('  * {}'.format(n.short_image))
    print('    {}'.format(_py2to3.text_repr(n.text)))
    print('')

print('Trying to get a source slice for two nodes in different units...')
try:
    print('  -> {}'.format(lal.Token.text_range(unit.first_token,
                                                unit2.last_token)))
    print('... got no error: unacceptable!')
except ValueError:
    print('... got the expected ValueError!')
print('')

print('Done.')
