with Pkg_G;

procedure Test is
   function Foo return Integer is
   begin
      return 42;
   end Foo;

   package Base is
      type T is tagged null record;

      procedure Bar (Self : T) is null;
   end Base;

   package Derived is
      type T is new Base.T with null record;

      overriding procedure Bar (Self : T) is null;
   end Derived;

   package Derived_2 is
      type T is new Derived.T with null record;
   end Derived_2;

   package Derived_3 is
      type T is new Derived_2.T with null record;

      overriding procedure Bar (Self : T) is null;
   end Derived_3;

   type F_T is access function return Integer;

   package Pkg_I is new Pkg_G (F => Foo);

   My_T : Base.T;
   X : Integer;
   Y : Base.T'Class := My_T;
   Z : F_T := Foo'Access;
   YY : Derived.T'Class;
begin
   X := Foo;
   X := Z.all;
   X := Pkg_I.Baz;
   Y.Bar;
   YY.Bar;
end Test;
