package Invalid is

   Int_Not : constant := not (-1);
   --% node.f_expr.p_eval_as_int

   type Enum is ('A', 'B');

   Enum_Neg : constant Enum := -'A';
   --% node.f_default_expr.p_eval_as_int

   Enum_Plus : constant Enum := +'A';
   --% node.f_default_expr.p_eval_as_int

   Enum_Abs : constant Enum := abs 'A';
   --% node.f_default_expr.p_eval_as_int

   Enum_Not : constant Enum := not 'A';
   --% node.f_default_expr.p_eval_as_int

end Invalid;
