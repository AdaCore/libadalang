package Unsigned is

   --  'Base of a modular type is its declared range 0 .. modulus - 1 (no
   --  widening): root type, subtype with a subrange, non-power-of-two
   --  modulus, and a derived type.

   type Mod8 is mod 256;

   Mod8_Base_First : constant Mod8 := Mod8'Base'First;
   --% node.f_default_expr.p_eval_as_int
   Mod8_Base_Last : constant Mod8 := Mod8'Base'Last;
   --% node.f_default_expr.p_eval_as_int

   --  A subtype constraint is ignored: 'Base spans the whole modulus
   subtype Mod8_Sub is Mod8 range 10 .. 20;

   Mod8_Sub_Base_First : constant Mod8 := Mod8_Sub'Base'First;
   --% node.f_default_expr.p_eval_as_int
   Mod8_Sub_Base_Last : constant Mod8 := Mod8_Sub'Base'Last;
   --% node.f_default_expr.p_eval_as_int

   type Mod100 is mod 100;

   Mod100_Base_Last : constant Mod100 := Mod100'Base'Last;
   --% node.f_default_expr.p_eval_as_int

   type Derived_Mod is new Mod8;

   Derived_Mod_Base_Last : constant Derived_Mod := Derived_Mod'Base'Last;
   --% node.f_default_expr.p_eval_as_int

end Unsigned;
