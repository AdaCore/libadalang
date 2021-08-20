package Test is
   function G return Integer;
   --% node.p_next_part_for_decl()

   function F return Boolean is
     (True);
   --% node.p_next_part_for_decl()

   I : constant Integer := 42;
   --% node.p_next_part_for_decl()
end Test;
