with Foo;

procedure Inherit is
   type U is new Foo.T with null record;

   --  Bar is inherited from Foo.T, so should be a primitive of U
   procedure Bar (Self : U);
   --% node.p_subp_spec_or_null().p_primitive_subp_tagged_type()

   --  But New_Bar is not, so the query should return None
   procedure New_Bar (Self : U);
   --% node.p_subp_spec_or_null().p_primitive_subp_tagged_type()
begin
   null;
end Inherit;
