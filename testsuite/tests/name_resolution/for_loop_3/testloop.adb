procedure Testloop is
   type My_Int is range Integer'First .. Integer'Last;
   B : My_Int;
begin
   for J in My_Int range 1 .. 58 loop
      B := J;
   end loop;
   pragma Test_Block;

end Testloop;
