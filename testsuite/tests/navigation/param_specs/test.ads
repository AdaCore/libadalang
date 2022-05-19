package Test is
   procedure Foo (X, Y : Integer);
   --% ids = node.f_subp_spec.f_subp_params.f_params[0].f_ids
   --% ids[0].p_next_part()
   --% ids[1].p_next_part()

   procedure Bar (X : Integer; Y : Integer);
   --% params = node.f_subp_spec.f_subp_params.f_params
   --% params[0].f_ids[0].p_next_part()
   --% params[1].f_ids[0].p_next_part()
private
   procedure Foo (X : Integer; Y : Integer) is null;
   --% params = node.f_subp_spec.f_subp_params.f_params
   --% params[0].f_ids[0].p_previous_part()
   --% params[1].f_ids[0].p_previous_part()

   procedure Bar (X, Y : Integer) is null;
   --% ids = node.f_subp_spec.f_subp_params.f_params[0].f_ids
   --% ids[0].p_previous_part()
   --% ids[1].p_previous_part()
   --% ids[0].p_canonical_part()
   --% ids[1].p_canonical_part()
end Test;

