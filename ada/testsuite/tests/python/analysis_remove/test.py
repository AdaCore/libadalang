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

ctx.remove('foo.adb')
try:
    ctx.remove('foo.adb')
except KeyError:
    print('Trying to remove the analysis unit for foo.adb twice fails the'
          ' second time, as expected')
else:
    assert False, ('Removing twice the analysis unit for foo.adb is supposed'
                   ' to raise an error but it did not')

print 'Done.'
