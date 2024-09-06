procedure Test is
   generic
   package Vectors is
      type U is null record;
      type U_Acc is access U;

      N : constant U_Acc := new U;
   end Vectors;

   generic
      with package Vec is new Vectors (<>);
   package Interface_G is
   end Interface_G;

   generic
   package Base_G is
      package Vec is new Vectors;

      generic
      package Inner_G is
         package Itf is new Interface_G (Vec);
      end Inner_G;
   end Base_G;

   generic
      with package Base is new Base_G (<>);
   package Pkg_G is
      package Inner is new Base.Inner_G;
      pragma Test_Statement;
   end Pkg_G;
begin
   null;
end Test;
