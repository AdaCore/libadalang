procedure Test is
   type A is new Natural;
   --% hex(node.p_discrete_range.low_bound.p_eval_as_int)
   --% hex(node.p_discrete_range.high_bound.p_eval_as_int)
   type B is new A range 2 .. A'Last;
   --% node.p_discrete_range
   subtype C is B range B'First + 2 .. B (A'Last - 2);
   --% node.p_discrete_range
   subtype D is B;
   --% node.p_discrete_range
begin
   null;
end Test;
