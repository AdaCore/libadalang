procedure Test is
   X : constant Integer := 12;

   Y : Integer renames X;

   Z : Integer := X;
   --% node.f_default_expr.p_is_static_expr()
   --% node.f_default_expr.p_eval_as_int
begin
   null;
end Test;
