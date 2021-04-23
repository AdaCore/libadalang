procedure Test is
   generic
   package Pkg_G is
      generic
      procedure Iterator_G;
   end Pkg_G;

   generic
   package Base_G is
      package Pkg_I is new Pkg_G;
   end Base_G;

   package body Pkg_G is
      procedure Iterator_G is null;
   end Pkg_G;

   package Base_I is new Base_G;

   procedure Foo is new Base_I.Pkg_I.Iterator_G;
begin
   Foo;
   pragma Test_Statement;
end Test;
