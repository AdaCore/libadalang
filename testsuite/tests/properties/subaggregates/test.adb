procedure Test is
   T : array (1 .. 2, 1 .. 2) of Character :=
      (('g', 'h'), (others => 'y'));
   --% root_aggr = node.f_default_expr
   --% root_aggr.p_is_subaggregate
   --% root_aggr.p_subaggregate_array_type

   --% sub_aggr_1 = root_aggr.f_assocs[0].f_r_expr
   --% sub_aggr_1.p_is_subaggregate
   --% sub_aggr_1.p_subaggregate_array_type
   --% sub_aggr_1.p_subaggregate_dimension

   --% sub_aggr_2 = root_aggr.f_assocs[1].f_r_expr
   --% sub_aggr_2.p_is_subaggregate
   --% sub_aggr_2.p_subaggregate_array_type
   --% sub_aggr_2.p_subaggregate_dimension
begin
   null;
end Test;
