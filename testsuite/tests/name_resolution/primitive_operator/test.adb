procedure Main is
   package Pkg is
      type T is tagged null record;

      function "&" (L : T; R : Integer) return T is (L);
   end Pkg;

   use Pkg;

   package Pkg_D is
      type My_T is new Pkg.T with null record;
   end Pkg_D;

   use type Pkg_D.My_T;

   X : T;
   Y : Pkg_D.My_T;
begin
   X := X & 2;
   pragma Test_Statement;
   Y := Y & 2;
   pragma Test_Statement;
end Main;
