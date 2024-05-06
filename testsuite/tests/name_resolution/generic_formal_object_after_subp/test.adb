procedure Test is
   package V is
      type T is tagged null record;

      function Bar (X : T) return Integer is (1);
   end V;
   generic
      with procedure Foo;
      X : V.T;
   package Pkg_G is
      Y : Integer := X.Bar;
   end Pkg_G;

   procedure F is null;

   K : V.T;

   package My_Pkg is new Pkg_G (F, K);
   pragma Test_Statement;
begin
   null;
end Test;
