procedure Test is
   generic
      with procedure Foo (X : Integer := 42);
   procedure Gen;

   procedure Gen is
   begin
      Foo (42);
      Foo (X => 42);
      Foo;
   end Gen;

   procedure Bar (Y : Integer) is null;

   procedure My_Gen is new Gen (Bar);
   --% body = node.p_designated_generic_decl.p_body_part()
   --% calls = [s.f_call for s in body.findall(lal.CallStmt)]
   --% calls[0].p_referenced_decl()
   --% calls[1].p_referenced_decl()
   --% calls[2].p_referenced_decl()
   --% calls[0].p_called_subp_spec
   --% calls[1].p_called_subp_spec
   --% calls[2].p_called_subp_spec
   --% calls[0].p_call_params
   --% calls[1].p_call_params
   --% calls[2].p_call_params
   --% calls[1].f_suffix[0].f_designator.p_referenced_decl()
begin
   null;
end Test;
