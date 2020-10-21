procedure Test1 is
   type Int is range 0 .. 10;
   type Int2 is range 0 .. 10;
   A : Int;
   function B return Int2;
   function B return Int;
begin
   A := B;
   pragma Test_Statement;
end Test1;
