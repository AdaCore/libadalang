import libadalang

src_buffer = """
limited with Ada.Text_IO;

procedure Foo is
   function \"+\" (S : String) return String is (S);
begin
   Ada.Text_IO.Put_Line (+\"Hello, world!\");
end Foo;
"""

ctx = libadalang.AnalysisContext()

unit = ctx.get_from_buffer('foo.adb', src_buffer)
assert unit, 'Could not create the analysis unit for foo.adb from a buffer'
print 'WithDecl: is_limited = {}'.format(unit.root.f_prelude[0].f_is_limited)

# Reparse, but without the "limited" keyword.
unit = ctx.get_from_buffer('foo.adb', src_buffer.split(' ', 1)[1])
assert unit, 'Could not reparse the analysis unit for foo.adb from a buffer'
print 'WithDecl: is_limited = {}'.format(unit.root.f_prelude[0].f_is_limited)

print 'Done.'
