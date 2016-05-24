procedure Test1 is
   type Int is range 0 .. 10;
   A : Int;
   B : Int;
begin
   A := B;
   pragma Test_Statement;

   A := 12;
   pragma Test_Statement;
end Test1;
