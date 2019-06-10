with Base;
with Pkg_G;

procedure Test is
   function Foo return Integer is
   begin
      return 42;
   end Foo;

   type F_T is access function return Integer;

   package Pkg_I is new Pkg_G (F => Foo);

   My_T : Base.T;
   X : Integer;
   Y : Base.T'Class := My_T;
   Z : F_T := Foo'Access;
begin
   X := Foo;
   X := Z.all;
   X := Pkg_I.Baz;
   Y.Bar;
end Test;
