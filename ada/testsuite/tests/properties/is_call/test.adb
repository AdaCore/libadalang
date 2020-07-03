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
      function Foo (X : Integer) return Integer is (F (X));
   end Pkg_G;

   package Pkg_I is new Pkg_G (Foo);
   package Pkg_J is new Pkg_G (Foo_I);

   function Bar return Integer is (42);

   type Int_Ptr is access all Integer;
   function Get_Ptr (X : Integer) return Int_Ptr is (null);
   function Get_Ptr_2 return Int_Ptr is (null);

   type Fun_Type_1 is access function return Integer;
   type Fun_Type_2 is access function (Z : Integer) return Integer;
   type Fun_Type_3 is access function return Int_Ptr;
   type Fun_Type_4 is access function (T : Integer := 0) return Integer;
   type Fun_Type_5 is access function (U : Integer := 0) return Fun_Type_4;
   type Proc_Type is access procedure (E, F : Integer);
   type Access_Fun_4 is access Fun_Type_5;

   function Get_Fun (X : Integer) return Fun_Type_2 is (Foo'Access);
   function Get_Fun_2 return Fun_Type_2 is (Foo'Access);
   function Get_Fun_3 return Fun_Type_1 is (null);

   type Access_Array_1 is array (Integer range 1 .. 2) of Fun_Type_1;
   type Access_Array_2 is array (Integer range 1 .. 2) of Fun_Type_2;
   type Access_Array_3 is array (Integer range 1 .. 2) of Proc_Type;

   Inst : T.TT;
   R : Integer;
   F : Fun_Type_1 := Bar'Access;
   G : Fun_Type_2 := Foo'Access;
   H : Fun_Type_3 := null;
   I : Fun_Type_4 := null;
   J : Fun_Type_5 := null;
   K : Access_Fun_4 := null;
   Arr_1 : Access_Array_1;
   Arr_2 : Access_Array_2;
   Arr_3 : Access_Array_3;
   Addr : System.Address;
begin
   R := Foo (3);
   Foo_Proc;

   Inst.Foo;
   T.Foo (Inst);

   R := Bar;
   R := Foo_I (3);
   R := F.all;
   R := G (2);
   R := G.all (2);
   R := H.all.all;

   R := I.all;
   R := I (2);
   R := I.all (2);

   R := J.all (2).all (3);
   R := J (U => 2) (T => 3);
   R := J.all.all;
   R := J.all (U => 2).all;
   R := J.all.all (T => 3);

   R := K.all.all.all;
   R := K.all (U => 2) (T => 3);

   R := Get_Ptr (2).all;
   R := Get_Ptr_2.all;
   R := Get_Fun (4)(2);
   R := Get_Fun (4).all (2);
   R := Get_Fun_2 (4);
   R := Get_Fun_2.all (4);
   R := Get_Fun_3.all;

   R := Arr_1 (1).all;
   R := Arr_2 (1) (12);
   R := Arr_2 (1).all (42);
   Arr_3 (1) (42, 12);

   R := Pkg_I.Foo (42);
   R := Foo_I (42);

   Addr := Bar'Address;
end Test;
