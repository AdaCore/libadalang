with B;
with B.C;

procedure Main is
   generic
      type U is private;
   package Pkg is
      package A_B_Inst is new B.C (U);
   end Pkg;

   package Pkg_Inst is new Pkg (Float);

   Val : B.Lol;
begin
   Val := Pkg_Inst.A_B_Inst.Bar;
   pragma Test_Statement;
end Main;
