with Ada.Text_IO;
with Bar;

procedure Foo is
begin
   Ada.Text_IO.Put_Line ("Hello, world!");
   Pragma Test (Bar.I);
end Foo;
