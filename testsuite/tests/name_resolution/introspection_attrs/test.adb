procedure Test is
   generic
      type T (<>) is private;
   package Pkg is
      A : Boolean := T'Has_Access_Values;
      pragma Test_Statement;

      B : Boolean := T'Has_Discriminants;
      pragma Test_Statement;

      C : Boolean := T'Has_Tagged_Values;
      pragma Test_Statement;

      D : Boolean := T'Definite;
      pragma Test_Statement;

      E : Boolean := T'Constrained;
      pragma Test_Statement;
   end Pkg;
begin
   null;
end Test;
