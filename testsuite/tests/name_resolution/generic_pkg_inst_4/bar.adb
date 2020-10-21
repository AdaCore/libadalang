with Foo;

procedure Bar is
   package Foo_Int is new Foo (Integer);
begin
   Foo_Int.X := 1;
   pragma Test_Statement;
end Bar;
