package Test is
   I, J : constant Integer;
   --% node.f_ids[0].p_next_part()
   --% node.f_ids[1].p_next_part()

   X : constant Integer;
   --% node.f_ids[0].p_next_part()
   Y : constant Integer;
   --% node.f_ids[0].p_next_part()
private
   I : constant Integer := 42;
   --% node.f_ids[0].p_previous_part()
   J : constant Integer := 42;
   --% node.f_ids[0].p_previous_part()

   X, Y : constant Integer := 42;
   --% node.f_ids[0].p_previous_part()
   --% node.f_ids[1].p_previous_part()
   --% node.f_ids[0].p_canonical_part()
   --% node.f_ids[1].p_canonical_part()
end Test;
