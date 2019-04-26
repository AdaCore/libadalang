procedure Test is
   generic
      type A is private;
   package Pkg_A_G is
      generic
         type B;
      procedure Foo (X : A);
   end Pkg_A_G;

   package body Pkg_A_G is
      procedure Foo (X : A) is
      begin
         null;
      end Foo;
   end Pkg_A_G;

   generic
      type B is private;
   package Pkg_B_G is
      package Pkg_A_I is new Pkg_A_G (A => B);
      procedure Foo is new Pkg_A_I.Foo (B => B);
   end Pkg_B_G;

   package Pkg_B_I is new Pkg_B_G (B => Integer);
begin
   Pkg_B_I.Foo (42);
   pragma Test_Statement;
end Test;
