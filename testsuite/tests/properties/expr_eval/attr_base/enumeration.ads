package Enumeration is

   --  'Base of an enumeration type is its declared range, spanning all the
   --  literals (exact). The cases use a derived type and a subtype of it to
   --  check that 'Base still spans every literal, ignoring the subtype's
   --  constraint.

   type Color is (Red, Green, Blue);
   type Derived_Color is new Color;

   Derived_Color_Base_First : constant Derived_Color :=
     Derived_Color'Base'First;
   --% node.f_default_expr.p_eval_as_int
   Derived_Color_Base_Last : constant Derived_Color :=
     Derived_Color'Base'Last;
   --% node.f_default_expr.p_eval_as_int

   subtype Color_Sub is Derived_Color range Green .. Blue;

   Color_Sub_Base_First : constant Derived_Color := Color_Sub'Base'First;
   --% node.f_default_expr.p_eval_as_int
   Color_Sub_Base_Last : constant Derived_Color := Color_Sub'Base'Last;
   --% node.f_default_expr.p_eval_as_int

end Enumeration;
