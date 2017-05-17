with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO;         use Ada.Text_IO;

procedure Foo is
   pragma Config (Display_Short_Images => True);
   pragma Test (Ada.Text_IO.Decimal_IO);
begin
   Put (Standard_Output, 1);
end Foo;
