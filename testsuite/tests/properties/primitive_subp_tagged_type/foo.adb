package body Foo is
   procedure Bar (Self : T) is null;
   --% $node.p_subp_spec_or_null().p_primitive_subp_tagged_type()

   type Test_Access is access procedure (Self : T);
   --% $node.f_type_def.f_subp_spec.p_primitive_subp_tagged_type()
end Foo;
