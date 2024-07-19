procedure Test is
   V : Integer := True;
   pragma Test_Statement (Expect_Fail => True);
begin
   V := 2.3;
   pragma Test_Statement (Expect_Fail => True);
end Test;
