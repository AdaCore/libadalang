procedure Test is
   T : array (1 .. 2, 1 .. 2) of Character :=
      (('g', 'h'), (others => 'y'));
   --% node.f_default_expr
   --% node.f_default_expr.p_is_subaggregate
   --% node.f_default_expr[1][0][1]
   --% node.f_default_expr[1][0][1].p_is_subaggregate
   --% node.f_default_expr[1][1][1]
   --% node.f_default_expr[1][1][1].p_is_subaggregate
begin
   null;
end Test;
