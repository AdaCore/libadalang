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

   M : Integer := Float'Machine_Radix;
   pragma Test_Statement;

   N : Integer := Duration'Small_Numerator;
   pragma Test_Statement;

   O : Integer := Duration'Small_Denominator;
   pragma Test_Statement;

   P : Boolean := Float'Machine_Overflows;
   pragma Test_Statement;

   Q : Float := Float'Scaling (A, 2);
   pragma Test_Statement;

   R : Boolean := Float'Machine_Rounds;
   pragma Test_Statement;

   S : Float := Float'Machine (1.0E+11);
   pragma Test_Statement;

   T : Float := Float'Compose(A, 2);
   pragma Test_Statement;

   U : Float := Float'Fraction (A);
   pragma Test_Statement;

   V : Integer := Float'Machine_Emin;
   pragma Test_Statement;

   W : Integer := Float'Machine_Emax;
   pragma Test_Statement;

   X : Float := Float'Safe_First;
   pragma Test_Statement;

   Y : Float := Float'Safe_Last;
   pragma Test_Statement;
begin
   null;
end Test_Float_Attrs;
