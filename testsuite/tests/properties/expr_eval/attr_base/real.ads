package Real is

   --  'Base of a real type: proper support is deferred. The base range should
   --  be the implementation-defined (widened) range but is currently (wrongly)
   --  the root type's declared range. With no eval_as_real yet, it is observed
   --  through a Boolean comparison, which still holds since the root range
   --  exceeds the subtype's 'Last.

   --  Floating point
   type Flt is digits 6;
   type Flt2 is new Flt range -1.0 .. 1.0;

   Flt_Base_Wider_Than_Last : constant Boolean := Flt2'Base'Last > Flt2'Last;
   --% node.f_default_expr.p_eval_as_int

   --  Fixed point
   type Fix is delta 0.01 range -10.0 .. 10.0;
   type Fix2 is new Fix range -1.0 .. 1.0;

   Fix_Base_Wider_Than_Last : constant Boolean := Fix2'Base'Last > Fix2'Last;
   --% node.f_default_expr.p_eval_as_int

end Real;
