package Valid is

   type Enum is ('A', 'B');

   C1 : constant Character := 'A';
   --% node.f_default_expr.p_eval_as_int

   C2 : constant Character := 'Z';
   --% node.f_default_expr.p_eval_as_int

   C3 : constant Character := C1;
   --% node.f_default_expr.p_eval_as_int

   E1 : constant Enum := 'A';
   --% node.f_default_expr.p_eval_as_int

   E2 : constant Enum := 'B';
   --% node.f_default_expr.p_eval_as_int

end Valid;
