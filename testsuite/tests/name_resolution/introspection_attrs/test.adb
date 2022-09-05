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

   type My_Record is record
      A, B : Integer;
   end record;

   Rec   : My_Record := (1, 2);
   Rec_V : Boolean := Rec'Valid_Scalars;
   pragma Test_Statement;
begin
   null;
end Test;
