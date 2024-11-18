procedure Test is
begin
   goto Missing;
   pragma Test_Statement (Expect_Fail => True);
end Test;
