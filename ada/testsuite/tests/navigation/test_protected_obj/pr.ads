package Pr is
   protected type A is
      entry Foo;
   private
      B : Integer;
   end A;
   --% $node.p_next_part_for_decl
   --% $node.p_body_part_for_decl
end Pr;
