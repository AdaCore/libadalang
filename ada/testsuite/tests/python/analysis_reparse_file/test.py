from __future__ import absolute_import, division, print_function

import os

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


def write_source(buffer):
    with open('foo.adb', 'wb') as f:
        f.write(buffer)

ctx = libadalang.AnalysisContext('iso-8859-1')


def check(unit):
    assert unit, 'Could not create the analysis unit for foo.adb from a file'
    if unit.diagnostics:
        print('Diagnostics for foo.adb:')
        for diag in unit.diagnostics:
            print('  {}'.format(diag))
    else:
        print('WithClause: has_limited = {}'.format(
            unit.root.f_prelude[0].f_has_limited
        ))


# First work with the "limited" keyword
write_source(src_buffer_1)

print('1. Parsing source 1')
unit = ctx.get_from_file('foo.adb', reparse=False)
check(unit)

# Now work without the "limited" keyword:
#  2. getting the unit without reparsing should preserve is_limited;
#  3. trying to reparse the deleted file should raise an error but preserve the
#     unit's tree;
#  4. getting the unit with reparsing should clear is_limited.
write_source(src_buffer_2)

print('2. Parsing source 2 (reparse=false)')
unit = ctx.get_from_file('foo.adb', reparse=False)
check(unit)

write_source(src_buffer_2)

print('3. Parsing source 2 (reparse=true)')
unit = ctx.get_from_file('foo.adb', reparse=True)
check(unit)

# Now restore the "limited" keyword in the soruce:
#  5. reparsing the unit should work and set is_limited;
#  6. reparsing the unit with a deleted file should wipe the AST and emit the
#     corresponding diagnostics.

write_source(src_buffer_1)

print('4. Reparsing source 1')
unit.reparse()
check(unit)

os.remove('foo.adb')

print('5. Reparsing with deleted file')
unit.reparse()
check(unit)

print('Done.')
