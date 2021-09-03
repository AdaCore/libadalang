package Pkg.Child is
   type U is new T with private;

   overriding procedure Foo (X : access U);

   procedure Main (X : access U);
private

   type U is new T with null record;
end Pkg.Child;
