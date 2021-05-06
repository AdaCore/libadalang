package Pkg is
   procedure Foo (X : String);
   --% node.p_next_part_for_decl()
   procedure Foo (X : Integer);
   --% node.p_next_part_for_decl()
end Pkg;
