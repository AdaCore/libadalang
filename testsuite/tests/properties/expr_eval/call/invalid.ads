package Invalid is

   I1 : constant Integer := Foo (1);
   --% node.f_default_expr.p_eval_as_int

   I2 : constant Integer := Integer ();
   --% node.f_default_expr.p_eval_as_int

   I3 : constant Integer := Integer (True);
   --% node.f_default_expr.p_eval_as_int

   B1 : constant Boolean := Boolean (1);
   --% node.f_default_expr.p_eval_as_int

   B2 : constant Boolean := Boolean (1.0);
   --% node.f_default_expr.p_eval_as_int

   type Rec is null record;

   R1 : constant Rec := Rec (True);
   --% node.f_default_expr.p_eval_as_int

end Invalid;
