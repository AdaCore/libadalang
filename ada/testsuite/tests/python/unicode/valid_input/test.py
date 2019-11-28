from __future__ import absolute_import, division, print_function

import libadalang
from libadalang._py2to3 import text_repr

from unicode_utils import (
    get_string_literal, src_buffer_iso_8859_1, src_buffer_utf_8
)


ctx = libadalang.AnalysisContext('iso-8859-1')


def check(unit):
    assert unit, 'Could not create the analysis unit from foo.adb'
    print('  Got: {}'.format(text_repr(get_string_literal(unit))))

# Check that at unit creation, we use the context-specific default
# charset.
print('1. Parsing buffer (a) with context default charset')
unit = ctx.get_from_buffer('foo.adb', src_buffer_iso_8859_1)
check(unit)

print('2. Reparsing buffer (b) with another charset')
unit = ctx.get_from_buffer('foo.adb', src_buffer_utf_8, charset='utf-8')
check(unit)

# Check that for reparsing, unit-specific charset takes precedence over the
# context-specific one.
print('3. Reparsing buffer (b) with the unit default charset')
unit = ctx.get_from_buffer('foo.adb', src_buffer_utf_8)
check(unit)

print('4. Reparsing buffer (a) with the original charset')
unit = ctx.get_from_buffer('foo.adb', src_buffer_iso_8859_1,
                           charset='iso-8859-1')
check(unit)

print('Done')
