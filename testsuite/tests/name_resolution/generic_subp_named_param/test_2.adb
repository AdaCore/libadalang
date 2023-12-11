procedure Test_2 is
   generic
      with procedure Foo (A : Integer);
   procedure G1;

   procedure G1 is
   begin
      Foo (A => 2);
   end G1;

   generic
      with procedure Bar (B : Integer);
   procedure G2;

   procedure G2 is
      procedure I1 is new G1 (Bar);
   begin
      I1;
   end G2;

   procedure Baz (C : Integer) is null;

   procedure I2 is new G2 (Baz);
   --% g2 = node.p_designated_generic_decl.p_body_part()
   --% g1_decl = g2.find(lal.GenericInstantiation).p_designated_generic_decl
   --% g1 = g1_decl.p_body_part()
   --% call = g1.find(lal.CallStmt).f_call
   --% call.p_referenced_decl()
   --% call.f_suffix[0].f_designator.p_referenced_decl()
   --% call.p_call_params
begin
   null;
end Test_2;

