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

print 'Done.'
