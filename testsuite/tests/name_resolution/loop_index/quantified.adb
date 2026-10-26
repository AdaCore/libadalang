procedure Quantified is
   type Int_Array is array (Integer range <>) of Integer;

   A : constant Int_Array (1 .. 3) := (10, 20, 30);
begin
   --  Loop_Index is only allowed on the loop parameter of a loop statement, not
   --  on the loop parameter of a quantified expression.

   pragma Assert (for all E of A => A (E'Loop_Index) = E);
   pragma Test_Statement (Expect_Fail => True);
end Quantified;
