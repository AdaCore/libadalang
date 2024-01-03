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
   pragma Test_Statement;
begin
   null;
end Test_3;
