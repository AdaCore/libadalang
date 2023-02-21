procedure Test is
   V : Integer := 1;
begin
   if V then
      null;
   end if;
   pragma Test_Statement;

   while True loop
      exit when V;
      pragma Test_Statement;
   end loop;
end Test;
