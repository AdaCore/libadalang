procedure Test is
   procedure Foo (X : Integer; Y : Boolean) is null;
begin
   Foo (True, 2 + 2);
   pragma Test_Statement (Expect_Fail => True);
end Test;
