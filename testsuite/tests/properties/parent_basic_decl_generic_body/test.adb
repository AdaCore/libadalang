procedure Test is
   generic
   package Pkg is
      procedure Foo;
      --% node.p_parent_basic_decl
      --% node.p_parent_basic_decl.p_parent_basic_decl
   end Pkg;

   package body Pkg is
      procedure Foo is null;
      --% node.p_parent_basic_decl
      --% node.p_parent_basic_decl.p_parent_basic_decl
   end Pkg;

   package My_Pkg is new Pkg;
   --% pkg_spec = node.p_designated_generic_decl
   --% pkg_body = pkg_spec.p_body_part
   --% pkg_spec.p_parent_basic_decl
   --% pkg_body.p_parent_basic_decl
   --% foo_spec = pkg_spec.find(lal.SubpDecl)
   --% foo_spec.p_parent_basic_decl
   --% foo_body = pkg_body.find(lal.NullSubpDecl)
   --% foo_body.p_parent_basic_decl

   generic
   procedure Bar;

   procedure Bar is null;

   procedure My_Bar is new Bar;
   --% bar_decl = node.p_designated_generic_decl
   --% bar_decl.p_parent_basic_decl
   --% bar_body = bar_decl.p_body_part()
   --% bar_body.p_parent_basic_decl
begin
   null;
end Test;
