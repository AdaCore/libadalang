procedure Test_Valid is
   type My_Bool is new Boolean;

   X, Y : My_Bool;
   Z : Integer;
begin
   if X and not Y then
      null;
   elsif X or Y then
      null;
   end if;

   while X and Y loop
      null;
   end loop;

   pragma Assert (X or Y);

   Z := (if X and not Y then 1 else 2);

   for K in 1 .. 3 loop
      exit when X and not Y;
   end loop;
end Test_Valid;
pragma Test_Block;
