with Ada.Text_IO; use Ada.Text_IO;

procedure A is
   function "+" (X : Integer) return Integer is
   begin
      Put_Line ("Hello");
      return X;
   end "+";

   X : Boolean;
begin
   X := (+1) > 0;
   pragma Test_Statement;
end A;
