procedure Test is
   function Foo (X : Integer) return Integer is (42);
   procedure Foo_Proc is
   begin
      null;
   end Foo_Proc;

   package T is
      type TT is tagged null record;
      procedure Foo (Self : TT) is null;
   end T;

   generic
      type T is private;
   function Foo_G (X : Integer) return Integer;

   function Foo_G (X : Integer) return Integer is (42);

   function Foo_I is new Foo_G (Integer);

   generic
      with function F (X : Integer) return Integer;
   package Pkg_G is
   end Pkg_G;

   package Pkg_I is new Pkg_G (Foo);
   package Pkg_I is new Pkg_G (Foo_I);

   function Bar return Integer is (42);
   type Fun_Type is access function return Integer;

   Inst : T.TT;
   R : Integer;
   F : Fun_Type := Bar'Access;
begin
   R := Foo (3);
   Foo_Proc;

   Inst.Foo;
   T.Foo (Inst);

   R := Foo_I (3);
   R := F.all;
end Test;
