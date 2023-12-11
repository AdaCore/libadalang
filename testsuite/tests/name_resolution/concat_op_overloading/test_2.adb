with Ada.Text_IO;

procedure Test_2 is
   function "&" (X, Y : String) return String is (X);

   A, B : constant String := "hello";

   C : String := "&" (A, B);
   pragma Test_Statement;
   D : String := A & B;
   pragma Test_Statement;
begin
   Ada.Text_IO.Put_Line (C);
   Ada.Text_IO.Put_Line (D);
end Test_2;
