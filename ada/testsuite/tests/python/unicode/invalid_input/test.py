import libadalang

from unicode_utils import *

ctx = libadalang.AnalysisContext('utf-8')
unit = ctx.get_from_buffer('foo.adb', src_buffer_iso_8859_1)
str_lit = get_string_literal(unit)

print 'Got: {}'.format(repr(str_lit))
print 'Done'
