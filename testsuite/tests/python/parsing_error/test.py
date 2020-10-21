import re

import libadalang


ADDR_RE = re.compile(r'0x[0-9a-f]+')


ctx = libadalang.AnalysisContext('iso-8859-1')
unit = ctx.get_from_file('foo.adb')


def print_heading(string):
    print('')
    print('#{}'.format(string))


print('# Printing diagnostics list (repr)')
print(repr(unit.diagnostics))
print('')


print('# Printing diagnostics list (str)')
print(str(unit.diagnostics))
print('')

print('# Printing each diagnostic individually (repr)')
for diag in unit.diagnostics:
    print(repr(diag))
print('')

print('# Printing each diagnostic individually (str)')
for diag in unit.diagnostics:
    print(str(diag))
print('')

print('Done.')
