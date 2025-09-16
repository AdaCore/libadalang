with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type Enum is (A, B, C);

   X : constant := 1;
   for Enum use
     (A => X,
      B => 1 + 1,
      C => 3);
   pragma Test_Statement;
begin
   null;
end Test;
