procedure Main is
   type Enum is (A, B, C);
   for Enum use (A => -1, B => 0, C => 2);

   type Enum2 is (E, F, G);

   type Enum3 is (H, I, J);
   for Enum3 use (H => 1, I => 2, J => 4);

   Var : Enum;
   X   : Integer;
begin
   X := Enum'Enum_Rep (Var);
   pragma Test_Statement;
   X := Enum2'Enum_Rep (E);
   pragma Test_Statement;
   X := Enum3'Enum_Rep (H);
   pragma Test_Statement;
   Var := Enum'Enum_Val (0);
   pragma Test_Statement;
end Main;
