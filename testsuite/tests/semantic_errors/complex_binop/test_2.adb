procedure Test is
   function Foo (X : Integer; Y : Integer) return Integer is (X);

   X : Integer;
begin
   X := Foo (2 + 2, 2 + True);
   pragma Test_Statement (Expect_Fail => True);
end Test;
