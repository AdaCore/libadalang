procedure Test1 is
   type Int is range 0 .. 10;
   type Int2 is range 0 .. 10;

   function B (P : Int2) return Int;
   function B (P : Int) return Int;
   function B (P : Int) return Int2;

   A : Int := 9;
   C : Int2 := 9;
begin
   begin
      A := B (A);
      C := B (A);
      A := B (C);
   end;
   pragma Test_Block;
end Test1;
