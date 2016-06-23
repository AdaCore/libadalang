procedure Test1 is
   type My_Integer is range 0 .. 1000;
   type My_Float is digits 12;

   function A return My_Integer;
   function A return My_Float;

   function B (I : My_Integer) return My_Integer;
   function B (F : My_Float) return My_Float;

   I : My_Integer;
begin
   I := B (A);
   pragma Test_Statement;
end Test1;
