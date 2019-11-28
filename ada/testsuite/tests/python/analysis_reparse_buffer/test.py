from __future__ import absolute_import, division, print_function

import libadalang


src_buffer_1 = b"""
limited with Ada.Text_IO;

procedure Foo is
   function \"+\" (S : String) return String is (S);
begin
   Ada.Text_IO.Put_Line (+\"Hello, world!\");
end Foo;
"""

src_buffer_2 = src_buffer_1.split(b' ', 1)[1]


def check(unit):
    assert unit, 'Could not create the analysis unit for foo.adb from a buffer'
    print('WithClause: has_limited = {}'.format(
        unit.root.f_prelude[0].f_has_limited
    ))

ctx = libadalang.AnalysisContext('iso-8859-1')

# Make sure the first parsing (with the "limited" keyword) works properly and
# check is_limited.
print('1. Parsing using buffer 1')
unit = ctx.get_from_buffer('foo.adb', src_buffer_1)
check(unit)

# Now make sure getting the unit with reparsing (without the "limited" keyword)
# clears is_limited.
print('2. Parsing using buffer 2')
unit = ctx.get_from_buffer('foo.adb', src_buffer_2)
check(unit)

# Finally make sure reparsing the unit (with the "limited" keyword) sets
# is_limited.
print('3. Reparsing using buffer 1')
unit.reparse(src_buffer_1)
check(unit)

print('Done.')
