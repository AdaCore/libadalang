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


def helper(reparse):
    unit = ctx.get_from_file('foo.adb', reparse)
    assert unit, 'Could not create the analysis unit for foo.adb from a e'
    print 'WithDecl: is_limited = {}'.format(
        unit.root.f_prelude[0].f_is_limited
    )


# First work with the "limited" keyword.
with open('foo.adb', 'w') as f:
    f.write(src_buffer)
helper(False)

# Now work without the "limited" keyword.
with open('foo.adb', 'w') as f:
    f.write(src_buffer.split(' ', 1)[1])
# Ask not to reparse first, then ask to reparse.
helper(False)
helper(True)

print 'Done.'
