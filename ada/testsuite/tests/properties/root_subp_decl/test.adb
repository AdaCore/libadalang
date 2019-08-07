procedure Test is
   package Pkg is
      type T is tagged null record;

      procedure Foo (X : T) is null;
      procedure Foo (X : T; Y : Integer) is null;
      function Foo return T is (null record);
   end Pkg;

   package Derived is
      type U is new Pkg.T with null record;

      overriding procedure Foo (X : U) is null;

      type R is new Pkg.T with null record;

      overriding procedure Foo (X : R) is null;
   end Derived;

   package Derived_2 is
      type V is new Derived.U with null record;

      overriding procedure Foo (X : V) is null;
      overriding procedure Foo (X : V; Y : Integer) is null;
      overriding function Foo return V is (null record);
   end Derived_2;
begin
   null;
end Test;
