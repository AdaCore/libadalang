separate (Pr)
protected body A is
   entry Foo when B > 0 is
   begin
      Boo := B;
      pragma Test_Statement;
   end Foo;
end A;
--% $node.p_previous_part.p_next_part_for_decl
