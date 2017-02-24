from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

import re

import libadalang


ADDR_RE = re.compile(r'0x[0-9a-f]+')


ctx = libadalang.AnalysisContext('iso-8859-1')
unit = ctx.get_from_file('foo.adb')


def print_heading(string):
    print('')
    print('#{}'.format(string))


def safe_print(obj, method):
    # Hide hexadecimal addresses since they will vary across runs/platforms
    string = method(obj)
    print(ADDR_RE.sub('0x...', string))


print('# Printing diagnostics list (repr)')
safe_print(unit.diagnostics, repr)
print('')

print('# Printing diagnostics list (str)')
safe_print(unit.diagnostics, str)
print('')

print('# Printing each diagnostic individually (repr)')
for diag in unit.diagnostics:
    safe_print(diag, repr)
print('')

print('# Printing each diagnostic individually (str)')
for diag in unit.diagnostics:
    safe_print(diag, str)
print('')

print('Done.')
