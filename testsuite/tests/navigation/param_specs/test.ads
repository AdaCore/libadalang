package Test is
   procedure Foo (X, Y : Integer);
   --% node.f_subp_spec[2][0][0][0][0].p_next_part()
   --% node.f_subp_spec[2][0][0][0][1].p_next_part()

   procedure Bar (X : Integer; Y : Integer);
   --% node.f_subp_spec[2][0][0][0][0].p_next_part()
   --% node.f_subp_spec[2][0][1][0][0].p_next_part()
private
   procedure Foo (X : Integer; Y : Integer) is null;
   --% node.f_subp_spec[2][0][0][0][0].p_previous_part()
   --% node.f_subp_spec[2][0][1][0][0].p_previous_part()

   procedure Bar (X, Y : Integer) is null;
   --% node.f_subp_spec[2][0][0][0][0].p_previous_part()
   --% node.f_subp_spec[2][0][0][0][1].p_previous_part()
   --% node.f_subp_spec[2][0][0][0][0].p_canonical_part()
   --% node.f_subp_spec[2][0][0][0][1].p_canonical_part()
end Test;

