procedure Test is
   type Arr is array (Positive range <>) of Integer;

   A : Arr := (1 .. 10 => True);
   pragma Test_Statement (Expect_Fail => True);

   B : Arr := (True .. False => 2);
   pragma Test_Statement (Expect_Fail => True);
begin
   null;
end Test;
