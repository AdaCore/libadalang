procedure Test1 is
   type Int is range 0 .. 10;
   type Int2 is range 0 .. 10;

   subtype Sub_Int is Int range 0 .. 5;

   A : Int;

   function B return Int2 is (0);
   function B return Sub_Int is (0);
begin
   A := B;
   pragma Test_Statement;
end Test1;
