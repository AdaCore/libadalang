package Valid is

   type Int is range -100 .. 100;
   subtype Int2 is Int;
   type Int3 is new Int2;

   subtype Nat is Int range 0 .. 100;
   type Pos is new Nat range 1 .. 100;

   I1 : constant Int := Int'First;
   --% node.f_default_expr.p_eval_as_int

   I2 : constant Int := Int'Last;
   --% node.f_default_expr.p_eval_as_int

   I3 : constant Int2 := Int2'Last;
   --% node.f_default_expr.p_eval_as_int

   I4 : constant Int3 := Int3'Last;
   --% node.f_default_expr.p_eval_as_int

   I5 : constant Nat := Nat'First;
   --% node.f_default_expr.p_eval_as_int

   I6 : constant Pos := Pos'First;
   --% node.f_default_expr.p_eval_as_int

   B1 : constant Boolean := Boolean'First;
   --% node.f_default_expr.p_eval_as_int

   B2 : constant Boolean := Boolean'Last;
   --% node.f_default_expr.p_eval_as_int

end Valid;
