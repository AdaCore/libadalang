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
   pragma Test_Statement;
begin
   null;
end Test_2;

