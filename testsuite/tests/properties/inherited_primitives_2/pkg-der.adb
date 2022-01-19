package body Pkg.Der is
   procedure Foo (X : U) is null;

   type D is new T with null record;
   --% node.p_get_primitives()

   procedure Foo (X : D);
   procedure Foo (X : D) is null;
end Pkg.Der;
