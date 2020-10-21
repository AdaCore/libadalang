package body Pkg1 is
   procedure Foo is
   begin
      null;
   end Foo;
   --% $node.p_decl_part()
end Pkg1;
--% $node.p_decl_part()

package body Pkg2 is
   type T is null record;
   --% $node.p_canonical_part()

   procedure Foo is
   begin
      null;
   end Foo;
   --% $node.p_decl_part()
end Pkg2;
--% $node.p_decl_part()
