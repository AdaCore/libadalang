procedure Test_Float_Attrs is
   A : Float := Float'Floor (12.0);
   pragma Test_Statement;

   B : Float := Float'Rounding (A);
   pragma Test_Statement;

   C : Float := Float'Ceiling (B + 15.0);
   pragma Test_Statement;

   D : Float := Float'Truncation (C + 0.120834);
   pragma Test_Statement;
begin
   null;
end Test_Float_Attrs;
