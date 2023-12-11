with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   Outer : Integer := 12;
begin
   A : Integer := 12;
   B : Integer := A;

   --  Basic test: A & B should be visible
   Put_Line (Integer'Image (A + B));
   pragma Test_Statement;

   --  Sequential visibility test: This should fail because C is not declared
   --  yet.
   Put_Line (Integer'Image (A + B + C));
   pragma Test_Statement;

   --  Renamings test: this should work
   C : Integer renames B;
   Put_Line (Integer'Image (A + B + C));
   pragma Test_Statement;

   --  FQN test
   Test.Outer := A;
   pragma Test_Statement;

   Outer : Integer := Test.Outer;

   --  Hiding test: should point to redefinition above
   Outer := A;
   pragma Test_Statement;

   --  FQN + hiding test: should point to redefinition above too
   Test.Outer := A;
   pragma Test_Statement;

end Test;
