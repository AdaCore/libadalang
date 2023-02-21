procedure Test is
   X : Integer := 2 + 2.4;
   pragma Test_Statement;
begin
   if X < 4.5 then
      null;
   end if;
   pragma Test_Statement;
end Test;

