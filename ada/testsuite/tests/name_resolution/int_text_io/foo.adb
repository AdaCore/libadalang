with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Text_IO.Decimal_IO;

procedure Foo is
   package Dec is new Ada.Text_IO.Decimal_IO (Float);
   pragma Test_Statement_UID;
begin
   Put (Standard_Output, 1);
end Foo;
