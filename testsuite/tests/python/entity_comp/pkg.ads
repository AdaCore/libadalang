package Pkg is

   type Prim_T is tagged null record;

   function Primitive (Self : Prim_T) return Integer
   is (Integer (12));

   type Der_T is new Prim_T with null record;

   function Prim_2 (Self : Der_T) return Integer is
     (Self.Primitive);

end Pkg;
