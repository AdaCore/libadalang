procedure Test is
   function Foo (X : Integer) return Integer is (X);
begin
   Foo (2);
   pragma Test_Statement (Expect_Fail => True);
end Test;
