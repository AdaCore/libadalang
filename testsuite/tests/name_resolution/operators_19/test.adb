procedure Test is
   type T is null record;

   X, Y : T;
begin
   X := X + Y;
   pragma Test_Statement (Expect_Fail => True);
   X := +X;
   pragma Test_Statement (Expect_Fail => True);
end Test;
