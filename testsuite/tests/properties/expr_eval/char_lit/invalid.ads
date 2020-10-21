package Invalid is

   type Enum is ('A', 'B');
   Value : constant Enum := 'C';
   --% node.f_default_expr.p_eval_as_int

   C1 : constant Character := No_Such_Entity;
   --% node.f_default_expr.p_eval_as_int

end Invalid;
