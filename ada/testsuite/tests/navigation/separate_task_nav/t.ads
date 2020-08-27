package T is
   task Tsk is
      entry B;
   end Tsk;
   --% $node.p_next_part_for_decl()
   --% $node.p_next_part_for_decl().p_next_part_for_decl()
   --% $node.p_next_part_for_decl().p_next_part_for_decl().p_previous_part()
   --% $node.p_next_part_for_decl().p_previous_part()
end T;
