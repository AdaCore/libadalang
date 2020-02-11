package Invalid is

   Int_Div_By_0 : constant := 2 / 0;
   --% node.f_expr.p_eval_as_int

   Int_Big_Pow : constant := 10 ** (10 ** 100);
   --% node.f_expr.p_eval_as_int

   Int_Neg_Pow : constant := 10 ** (-1);
   --% node.f_expr.p_eval_as_int

   Int_And_Then : constant Integer := 10 and then 20;
   --% node.f_default_expr.p_eval_as_int

   Inconsistent_Types : constant := 2.0 + 3;
   --% node.f_expr.p_eval_as_int

   Invalid_Types : constant := True + True;
   --% node.f_expr.p_eval_as_int

   Incomplete_Binop : constant := 1 +;
   --% node.f_expr.p_eval_as_int

end Invalid;
