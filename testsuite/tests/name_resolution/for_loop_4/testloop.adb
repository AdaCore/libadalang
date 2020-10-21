procedure Testloop is
   type My_Int is range Integer'First .. Integer'Last;
   B : My_Int;

   type My_Array is array (Integer range <>) of Natural;

   function Foo return My_Array is ((1, 2, 3, 4));

begin
   for El of Foo loop
      El := 12;
   end loop;
   pragma Test_Block;

end Testloop;
