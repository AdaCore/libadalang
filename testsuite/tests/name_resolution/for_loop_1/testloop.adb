procedure Testloop is
   A : Integer;
   type My_Int is range Integer'First .. Integer'Last;
   B : My_Int;
begin
   for J in 1 .. 100 loop
      A := J;
      pragma Test_Statement;
      B := J;
   end loop;

end Testloop;
