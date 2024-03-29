procedure Test is
begin
   Foo.Bar;
   pragma Test_Statement (Expect_Fail => True);
end Test;
