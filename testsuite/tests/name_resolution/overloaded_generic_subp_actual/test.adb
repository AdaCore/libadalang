procedure Test is
   generic
      with procedure Foo (X : Integer);
   procedure Gen;

   procedure Gen is
   begin
      Foo (42);
   end Gen;

   --  Define three potential candidates for the actual in the instantiation
   --  below.
   procedure Bar (X : Boolean) is null;
   procedure Bar (X : Integer) is null;
   procedure Bar (X : Float) is null;

   procedure My_Gen is new Gen (Foo => Bar);
   --% gen = node.p_designated_generic_decl.p_body_part()
   --% gen.find(lal.CallExpr).f_name.p_referenced_decl()
begin
   null;
end Test;
