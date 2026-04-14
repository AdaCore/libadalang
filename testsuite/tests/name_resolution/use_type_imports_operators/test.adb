procedure Test is
   package Pkg is
      type T is null record;

      procedure Foo (X : T) is null;
      function "+" (X : T) return T is (X);
   end Pkg;

   package Other is
      procedure Foo (X : Pkg.T) is null;
   end Other;

   use type Pkg.T;
   use Other;

   X : Pkg.T;
begin
   Foo (X);
   pragma Test_Statement;
end Test;
