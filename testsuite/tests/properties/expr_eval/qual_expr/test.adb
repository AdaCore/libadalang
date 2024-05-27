procedure Test is
   Qual_Expr_1 : constant Natural := String'("A" & "B")'Length;
   --% node.f_default_expr.p_eval_as_int

   Qual_Expr_2 : constant String := String'("A" & "B");
   --% node.f_default_expr.p_eval_as_string

   Qual_Expr_3 : constant Natural := Integer'(1 + 4);
   --% node.f_default_expr.p_eval_as_int

   Qual_Expr_4 : constant Character := Character'('A');
   --% node.f_default_expr.p_eval_as_int
begin
   null;
end Test;
