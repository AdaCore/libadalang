procedure Test is
   X : Integer;

   function Foo (X : Integer; Y : Boolean) return Integer is (0);
begin
   X := Foo (2);
   pragma Test_Statement (Expect_Fail => True);
end Test;
