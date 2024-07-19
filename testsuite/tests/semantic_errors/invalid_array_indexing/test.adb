procedure Test is
   type Arr is array (Positive range <>) of Integer;

   A : Arr := (1 .. 10 => 0);
   B : Boolean := True;
begin
   A (2) := B;
   pragma Test_Statement (Expect_Fail => True);

   B := A (2);
   pragma Test_Statement (Expect_Fail => True);

   A (B) := 2;
   pragma Test_Statement (Expect_Fail => True);
end Test;
