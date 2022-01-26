with Ada.Text_IO; use Ada.Text_IO;
procedure Test is
   package Itfs is
      type I is interface;
      --% node.p_get_primitives()

      procedure Foo (X : I) is abstract;

      type I2 is interface;
      --% node.p_get_primitives()

      procedure Foo (X : I2) is null;

      type I3 is interface and I and I2;
      --% node.p_get_primitives()
   end Itfs;

   package Concr is
      type T is tagged null record;

      procedure Foo (X : T) is null;

      type R is tagged null record;

      procedure Foo (X : R) is null;

      type S is tagged null record;
   end Concr;

   package Der is
      type U is new Concr.T and Itfs.I and Itfs.I2 with null record;
      --% node.p_get_primitives()

      type V is new Concr.T and Itfs.I3 with null record;
      --% node.p_get_primitives()

      type W is new Itfs.I2 with null record;
      --% node.p_get_primitives()
   end Der;

   package Itfs_Der is
      type J is interface and Itfs.I;
      --% node.p_get_primitives()

      procedure Foo (X : J) is abstract;
   end Itfs_Der;

   X : Der.U;
begin
   X.Foo;
end Test;
