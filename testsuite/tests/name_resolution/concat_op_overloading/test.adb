with Ada.Text_IO;
with Pkg;

procedure Test is
   use Pkg;

   A, B : constant String := "hello";

   C : String := "&" (A, B);
   pragma Test_Statement;
   D : String := A & B;
   pragma Test_Statement;
begin
   Ada.Text_IO.Put_Line (C);
   Ada.Text_IO.Put_Line (D);
end Test;
