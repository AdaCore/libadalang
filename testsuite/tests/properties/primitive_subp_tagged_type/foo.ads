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
private
   procedure Baz (Self : T);
    --% $node.p_subp_spec_or_null().p_primitive_subp_tagged_type()
end Foo;
