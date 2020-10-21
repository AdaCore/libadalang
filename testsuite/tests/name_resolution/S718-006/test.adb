procedure Test is
   package Pkg is
      type T is tagged null record;
      function Foo (X : T) return Integer is (2);
   end Pkg;

   package Pkg_2 is
      type Fun is access function (X : Pkg.T) return Integer;
      type R is tagged null record;
      subtype U is R;
      function Foo (X : U) return Fun is (null);
   end Pkg_2;

   use Pkg_2;
   use Pkg;

   X : T;
   Y : Integer;
begin
   Y := Foo (T'(X));
   pragma Test_Statement;
end Test;
