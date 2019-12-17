separate (Pkg)
procedure Foo is
begin
   null;
end Foo;
--% $node.p_decl_part

separate (Pkg)
procedure Bar is
begin
   null;
end Bar;
--% $node.p_decl_part

package Baz is
   procedure Bazz;
   --% $node.p_body_part
end Baz;

package body Baz is
   procedure Bazz is separate;
   --% $node.p_previous_part
   --% $node.p_next_part_for_decl
end Baz;

separate (Baz)
procedure Bazz is
begin
   null;
end Bazz;
--% $node.p_previous_part
--% $node.p_decl_part
