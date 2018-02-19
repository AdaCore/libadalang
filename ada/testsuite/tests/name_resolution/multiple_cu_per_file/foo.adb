with Ada.Text_IO; use Ada.Text_IO;
with Pkg1, Pkg2;

procedure Foo is
   I1 : constant Integer := Pkg1.Fact (5);
   pragma Test_Statement;

   I2 : constant Integer := Pkg2.Id (6);
   pragma Test_Statement;
begin
   Put_Line ("Fact (5) =" & Integer'Image (I1));
   Put_Line ("Id (6) =" & Integer'Image (I2));
end Foo;
