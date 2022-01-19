package Pkg is
   type T is abstract tagged private;
   --% node.p_get_primitives()

   procedure Foo (X : T) is abstract;

private
   type T is abstract tagged null record;
   --% node.p_get_primitives()

   function Priv (X : T) return Integer;
end Pkg;
