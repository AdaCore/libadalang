package Invalid is

   Inconsistent_Type : constant Integer := Integer'Max (1, 1.0);
   --% node.f_default_expr.p_eval_as_int

   For_Enum : constant Boolean := Boolean'Min (True, False);
   --% node.f_default_expr.p_eval_as_int

end Invalid;
