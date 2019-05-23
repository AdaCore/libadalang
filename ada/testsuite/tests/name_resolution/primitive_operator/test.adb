procedure Main is
   package Pkg is
      type T is tagged null record;

      function "&" (L : T; R : Integer) return T is (L);
   end Pkg;

   use Pkg;

   type My_T is new T with null record;

   X : T;
begin
   X := X & 2;
   pragma Test_Statement;
end Main;
