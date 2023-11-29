--  Check that it also works when parameter packing doesn't match between
--  the formal and the actual.
procedure Test_3 is
   generic
      with procedure Foo (X, Y : Integer);
   procedure Gen;

   procedure Gen is
   begin
      Foo (Y => 1, X => 2);
   end Gen;

   procedure Bar (A : Integer; B : Integer) is null;

   procedure My_Gen is new Gen (Bar);
   --% body = node.p_designated_generic_decl.p_body_part()
   --% calls = [s.f_call for s in body.findall(lal.CallStmt)]
   --% calls[0].p_referenced_decl()
   --% calls[0].p_called_subp_spec
   --% calls[0].p_call_params
   --% calls[0].f_suffix[0].f_designator.p_referenced_decl()
   --% calls[0].f_suffix[1].f_designator.p_referenced_decl()
begin
   null;
end Test_3;
