package Pkg.Der is
   type U is tagged private;
   --% node.p_get_primitives()

   procedure Bar (X : U);
   procedure Foo (X : U);

private
   type U is new T with null record;
   --% node.p_get_primitives()

   procedure Bar (X : U) is null;
end Pkg.Der;
