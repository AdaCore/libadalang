package Pkg is
   type A is null record;
   type B is null record;

   function Foo (X : A) return B is (null record);
end Pkg;
