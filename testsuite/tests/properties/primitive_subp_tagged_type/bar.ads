package Bar is
   type T is tagged private;
   generic
      type U is tagged private;
      with procedure Foo (X : T);
      --% node.p_subp_spec_or_null().p_primitive_subp_tagged_type()
      with procedure Foo (X : U);
      --% node.p_subp_spec_or_null().p_primitive_subp_tagged_type()
   package Pkg is
   end Pkg;

   procedure Foo;
end Bar;
