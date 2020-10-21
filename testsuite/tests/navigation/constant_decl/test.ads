package Test is
   X : constant Integer;
   --% node.p_private_part_decl

private
   X : constant Integer := 2;
   --% node.p_public_part_decl
end Test;
