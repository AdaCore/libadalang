package Invalid is

   Enum_Succ : constant Boolean := Boolean'Succ (True);
   --% node.f_default_expr.p_eval_as_int

   Enum_Pred : constant Boolean := Boolean'Pred (False);
   --% node.f_default_expr.p_eval_as_int

   No_Arg : constant Boolean := Boolean'Succ;
   --% node.f_default_expr.p_eval_as_int

   Two_Args : constant Boolean := Boolean'Succ (True, False);
   --% node.f_default_expr.p_eval_as_int

end Invalid;
