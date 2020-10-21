package body Pkg4 is
   package Nested1 is
      procedure Foo;
   end Nested1;

   package body Nested1 is separate;

   procedure Foo is
   begin
      Nested1.Foo;
   end Foo;
end Pkg4;
