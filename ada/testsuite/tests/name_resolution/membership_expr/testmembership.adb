procedure Testmembership is
   A, B : Integer := 12;
begin
   if A in 1 | 2 | 5 .. B then
      null;
   end if;
end Testmembership;
pragma Test_Block;
