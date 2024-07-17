procedure Test is
   X : Integer := 2;
   V : String := "ab" & X;
   pragma Test_Statement (Expect_Fail => True);
begin
   null;
end Test;
