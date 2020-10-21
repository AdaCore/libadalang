procedure Main is
   generic
   package Pkg is
      generic package NPkg is
         type T is null record;
         function F (L : T) return T is (L);
      end NPkg;
   end Pkg;

   generic
   package Pkg2 is
      package P is new Pkg;
      package PP is new P.NPkg;
      type U is new PP.T;
   end Pkg2;

   package Pkg2_Inst is new Pkg2;

   generic
   package Pkg3 is
      type V is new Pkg2_Inst.U;
   end Pkg3;

   package Pkg3_Inst is new Pkg3;


   X : Pkg3_Inst.V;
begin
   X := Pkg3_Inst.F (X);
   pragma Test_Statement;
end Main;
