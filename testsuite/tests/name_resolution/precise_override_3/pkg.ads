package Pkg is
   type T is abstract tagged private;

   procedure Foo (X : T) is abstract;

   type U is new T with private;

   overriding procedure Foo (X : U) is null;

   procedure Bar;
private
   type T is abstract tagged null record;
   type U is new T with null record;
end Pkg;
