package Pkg is
   function Foo (X : Boolean) return Integer is (0);
   function Foo (X : Integer) return Boolean is (False);
end Pkg;
