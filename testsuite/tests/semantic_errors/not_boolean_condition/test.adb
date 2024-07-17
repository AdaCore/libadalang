procedure Test is
   V : Integer := 1;
begin
   if V then
      null;
   end if;
   pragma Test_Statement (Expect_Fail => True);

   while True loop
      exit when V;
      pragma Test_Statement (Expect_Fail => True);
   end loop;
end Test;
