package Valid is

   S1 : constant String := "Hello World!";
   --% node.f_default_expr.p_eval_as_string

   S2 : constant String := "Hello";
   --% node.f_default_expr.p_eval_as_string

   S3 : constant String := "World!";
   --% node.f_default_expr.p_eval_as_string

   S4 : constant String := S2 & " " & S3;
   --% node.f_default_expr.p_eval_as_string
end Valid;
