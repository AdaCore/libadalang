package Invalid is

   Enum_Succ : constant Boolean := Boolean'Succ (True);
   --% node.f_default_expr.p_eval_as_int

   Enum_Pred : constant Boolean := Boolean'Pred (False);
   --% node.f_default_expr.p_eval_as_int

end Invalid;
