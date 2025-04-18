--  Check that it also works when parameter packing doesn't match between
--  the formal and the actual.
procedure Test_3 is
   generic
      with procedure Foo (X, Y : Integer);
      with procedure Foo_2 (X : Integer; Y : Integer);
   procedure Gen;

   procedure Gen is
   begin
      Foo (Y => 1, X => 2);
      Foo_2 (Y => 1, X => 2);
   end Gen;

   procedure Bar (A : Integer; B : Integer) is null;
   procedure Bar_2 (A, B : Integer) is null;

   procedure My_Gen is new Gen (Bar, Bar_2);
   pragma Test_Statement;
begin
   null;
end Test_3;
