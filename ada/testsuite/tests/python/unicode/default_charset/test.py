from __future__ import absolute_import, division, print_function

import libadalang
from libadalang._py2to3 import text_repr

from unicode_utils import get_string_literal, src_buffer_iso_8859_1


ctx = libadalang.AnalysisContext()

# Check that at unit creation, we use the context-specific default
# charset, which is itself the default charset for libadalang (iso-8859-1).
print('Parsing a buffer with the language-specific default charset...')
unit = ctx.get_from_buffer('foo.adb', src_buffer_iso_8859_1)
assert unit, 'Could not create the analysis unit from foo.adb'
print('Got: {}'.format(text_repr(get_string_literal(unit))))
print('Done')
