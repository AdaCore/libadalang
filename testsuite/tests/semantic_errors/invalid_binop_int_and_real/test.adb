procedure Test is
   X : Integer := 2 + 2.4;
   pragma Test_Statement (Expect_Fail => True);
begin
   if X < 4.5 then
      null;
   end if;
   pragma Test_Statement (Expect_Fail => True);
end Test;

