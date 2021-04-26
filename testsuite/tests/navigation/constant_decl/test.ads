package Test is
   X : constant Integer;
   --% node.p_private_part_decl
   --% node.p_next_part_for_decl()

private
   X : constant Integer := 2;
   --% node.p_public_part_decl
   --% node.p_previous_part_for_decl()
   --% node.p_canonical_part()
end Test;
