package Foo is

   type U is null record;

   procedure Foo (Self : U);
   --% $node.p_subp_spec_or_null().p_primitive_subp_tagged_type()

   type T is tagged null record;
   procedure Bar (Self : T);
   --% $node.p_subp_spec_or_null().p_primitive_subp_tagged_type()

   package Boo is
      procedure Foo (Self : T);
      --% $node.p_subp_spec_or_null().p_primitive_subp_tagged_type()
   end Boo;

   generic
      type U is private;
   procedure Dummy (Self : T);
   --% node.f_subp_decl.f_subp_spec.p_primitive_subp_tagged_type()

private
   procedure Baz (Self : T);
    --% $node.p_subp_spec_or_null().p_primitive_subp_tagged_type()
end Foo;
