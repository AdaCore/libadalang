procedure Test is
   type Arr is array (1 .. 10) of Integer;

   function Foo return Arr is ((others => 0));
   procedure Foo is null;
begin
   for X of Foo loop
      null;
   end loop;
   pragma Test_Block;
end Test;
