procedure Test is
   generic
      type T is private;
   package Pkg_G is
   end Pkg_G;

   generic
      with package F is new Pkg_G (<>);
   package Lol is
      use F;
      X : T;
      procedure Foo (X : T) is null;
   end Lol;

   package Pkg_I is new Pkg_G (Integer);
   package Lol_I is new Lol (Pkg_I);
begin
   Lol_I.Foo (2);
   pragma Test_Statement;
   Lol_I.X := 3;
   pragma Test_Statement;
end Test;
