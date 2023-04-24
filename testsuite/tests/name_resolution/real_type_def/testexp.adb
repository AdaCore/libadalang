procedure Testexp is
   Cst_R : constant := 3.1415;
   Cst_I : constant := 2;

   type Floating_Point_Def is digits Cst_I range 0.0 .. 2.0 * Cst_R;
   pragma Test_Statement;

   type Floating_Point_Def_2 is digits Cst_I;
   pragma Test_Statement;

   type Ordinary_Fixed_Point_Def is delta Cst_R range 0.0 .. 2.0 * Cst_R;
   pragma Test_Statement;

   type Ordinary_Fixed_Point_Def_2 is delta Cst_R;
   pragma Test_Statement;

   type Decimal_Fixed_Point_Def is
     delta Cst_R digits Cst_I range 0.0 .. 2.0 * Cst_R;
   pragma Test_Statement;

   type Decimal_Fixed_Point_Def_2 is delta Cst_R digits Cst_I;
   pragma Test_Statement;
begin
   null;
end Testexp;
