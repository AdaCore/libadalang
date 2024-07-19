procedure Test is
   type T is tagged null record;

   X : access Integer;
   Y : array (1 .. 10) of Integer;
   Z : access T'Class;
begin
   X := 2;
   pragma Test_Statement (Expect_Fail => True);

   Y := 3;
   pragma Test_Statement (Expect_Fail => True);

   Z := 4;
   pragma Test_Statement (Expect_Fail => True);
end Test;
