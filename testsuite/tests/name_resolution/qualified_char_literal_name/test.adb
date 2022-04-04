procedure Test is
   package Pkg is
      type E is ('A', B);
   end Pkg;
   use Pkg;
begin
   if Pkg.'A' = B then
      null;
   end if;
   pragma Test_Statement;
end Test;
