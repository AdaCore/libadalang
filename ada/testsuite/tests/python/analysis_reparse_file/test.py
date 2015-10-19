import os

import libadalang


src_buffer_1 = """
limited with Ada.Text_IO;

procedure Foo is
   function \"+\" (S : String) return String is (S);
begin
   Ada.Text_IO.Put_Line (+\"Hello, world!\");
end Foo;
"""

src_buffer_2 = src_buffer_1.split(' ', 1)[1]


def write_source(buffer):
    with open('foo.adb', 'w') as f:
        f.write(buffer)

ctx = libadalang.AnalysisContext('iso-8859-1')


def check(unit):
    assert unit, 'Could not create the analysis unit for foo.adb from a e'
    print 'WithDecl: is_limited = {}'.format(
        unit.root.f_prelude[0].f_is_limited
    )


# First work with the "limited" keyword.
write_source(src_buffer_1)

print '1. Parsing source 1'
unit = ctx.get_from_file('foo.adb', reparse=False)
check(unit)

# Now work without the "limited" keyword:
#  2. getting the unit without reparsing should preserve is_limited;
#  3. trying to reparse the deleted file should raise an error but preserve the
#     unit's tree;
#  4. getting the unit with reparsing should clear is_limited.
write_source(src_buffer_2)

print '2. Parsing source 2 (reparse=false)'
unit = ctx.get_from_file('foo.adb', reparse=False)
check(unit)

os.remove('foo.adb')

print '3. Parsing with deleted file (reparse=true)'
try:
    ctx.get_from_file('foo.adb', reparse=True)
except IOError:
    check(unit)
else:
    assert False, (
        'Reparsing analysis unit from a deleted file returned something!'
    )

write_source(src_buffer_2)

print '4. Parsing source 2 (reparse=true)'
unit = ctx.get_from_file('foo.adb', reparse=True)
check(unit)

# Now restore the "limited" keyword in the soruce:
#  5. reparsing the unit should work and set is_limited;
#  6. reparsing the unit with a deleted file should raise an error but preserve
#     the unit's tree.

write_source(src_buffer_1)

print '5. Reparsing source 1'
unit.reparse()
check(unit)

os.remove('foo.adb')

print '6. Reparsing with deleted file'
try:
    unit.reparse()
except IOError:
    check(unit)
else:
    assert False, 'Reparding a deleted file is supposed to raise an exception'

print 'Done.'
