procedure Test is
   X : Integer := Foo (True);
   pragma Test_Statement (Expect_Fail => True);
begin
   null;
end Test;
