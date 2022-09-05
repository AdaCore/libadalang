package Pkg.Der is
   type U is new T with null record;

   overriding procedure Foo (X : U) is null;
end Pkg.Der;
