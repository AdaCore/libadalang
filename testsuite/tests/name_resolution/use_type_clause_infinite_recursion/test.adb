procedure Test is
   generic
   package Pkg is
      type T is tagged null record;
      type T_Access is access all T;

      function Foo (X : access T) return T is (null record);

      X : constant T_Access := new T;
   end Pkg;

   package Helper is
      package My_Pkg is new Pkg;

      subtype My_T is My_Pkg.T;

      use type My_T;
   end Helper;

   use Helper;

   X : My_T := My_Pkg.X.Foo;
   pragma Test_Statement;
begin
   null;
end Test;

