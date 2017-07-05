package Pouet is

   package Foo is
      A : Integer := 12;
   end Foo;

   package Bar is
      use Foo;

      B : Integer := A;
   end Bar;

   package Baz is
      use Bar;
      C : Integer := A;

      pragma Test (B);
      pragma Test (A);
   end Baz;

end Pouet;
