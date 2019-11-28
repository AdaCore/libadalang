from __future__ import absolute_import, division, print_function

import libadalang


src_buffer = b"""
limited with Ada.Text_IO;

procedure Foo is
   function \"+\" (S : String) return String is (S);
begin
   Ada.Text_IO.Put_Line (+\"Hello, world!\");
end Foo;
"""

ctx = libadalang.AnalysisContext('iso-8859-1')
unit = ctx.get_from_buffer('foo.adb', src_buffer)
assert unit, 'Could not create the analysis unit for foo.adb from a buffer'

print('Done.')
