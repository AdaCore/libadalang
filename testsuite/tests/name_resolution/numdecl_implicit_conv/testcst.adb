procedure Test is
   A : constant := 2 * 15.0;
   B : constant := A / 2;
begin
   if B = 15.0 then
      null;
   end if;
   pragma Test_Statement;
end Test;
