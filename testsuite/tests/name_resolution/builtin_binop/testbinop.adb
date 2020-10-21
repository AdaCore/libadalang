procedure Testbinop is
   package Pkg is
      type Typ is range 1 .. 1000;
   end Pkg;

   A, B : Pkg.Typ := 10;
begin
   A := Pkg."+" (A, B);

   if Pkg."=" (A, B) then
      null;
   end if;
end Testbinop;
pragma Test_Block;
