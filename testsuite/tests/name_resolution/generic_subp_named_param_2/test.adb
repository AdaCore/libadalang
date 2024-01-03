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
   pragma Test_Statement;
begin
   null;
end Test;
