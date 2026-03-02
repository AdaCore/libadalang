procedure Test is
   function Foo (X : Integer) return Integer is (X)
      with Pre => (L1 => X > 0);
   pragma Test_Block (Expect_Fail => True);
begin
end Test;
