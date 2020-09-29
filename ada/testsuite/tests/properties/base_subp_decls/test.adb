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

   package Interfaces is
      type I is interface;
      type J is interface;

      procedure Foo (X : I) is abstract;
      procedure Foo (X : J) is abstract;
   end Interfaces;

   package Interfaces_2 is
      type K is interface and Interfaces.I and Interfaces.J;

      overriding procedure Foo (X : K) is null;
   end Interfaces_2;

   package Derived_4 is
      type D is new Derived_2.V and Interfaces_2.K with null record;

      overriding procedure Foo (X : D) is null;
   end Derived_4;

   package T925_013_Base is
      type T is tagged null record;
      function Foo (X : T) return T'Class is (X);
   end T925_013_Base;

   package T925_013_Derived is
      type U is new T925_013_Base.T with null record;
      overriding function Foo (X : U) return T925_013_Base.T'Class is (X);
   end T925_013_Derived;

   package T925_015 is
      type T is tagged null record;

      function Foo (X : T) return T;
   private
      function Foo (X : T) return T is (X);
   end T925_015;
begin
   null;
end Test;
