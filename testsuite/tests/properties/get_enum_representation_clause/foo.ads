package Foo is
   type T is (A, B, C);
   --% $node.p_get_enum_representation_clause()

   X : Integer;  --  something in the middle

   for T use (A => 1, B => 42, C => 666);
end Foo;
