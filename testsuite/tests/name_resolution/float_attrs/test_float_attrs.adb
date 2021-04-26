procedure Test_Float_Attrs is
   A : Float := Float'Floor (12.0);
   pragma Test_Statement;

   B : Float := Float'Rounding (A);
   pragma Test_Statement;

   C : Float := Float'Ceiling (B + 15.0);
   pragma Test_Statement;

   D : Float := Float'Truncation (C + 0.120834);
   pragma Test_Statement;

   E : Float := Float'Copy_Sign (C, D);
   pragma Test_Statement;

   F : Float := Float'Remainder (D, E);
   pragma Test_Statement;

   G : Float := Float'Adjacent (E, F);
   pragma Test_Statement;

   H : Float := Float'Invalid_Value;
   pragma Test_Statement;

   I : Float := Float'Model_Small;
   pragma Test_Statement;

   J : Float := Float'Model_Epsilon;
   pragma Test_Statement;

   K : Float := Float'Machine_Rounding (A);
   pragma Test_Statement;

   L : Integer := Float'Exponent (A);
   pragma Test_Statement;

   M : Integer := Float'Machine_Radix (A);
   pragma Test_Statement;
begin
   null;
end Test_Float_Attrs;
