package Renam is
   type T is record
      V : Integer;
   end record;

   MyT : T := (V => 0);

   procedure P;
   procedure Q renames P;

   Un : constant Integer := 1;
   One : Integer renames Un;
   --% node.p_is_constant_object

   --  TODO: Add the following test when Ada2022 optional subtype_mark
   --  in object renames is supported by libadalang.

   --  Deux : constant := 2;
   --  Two renames Deux;
   --  --% node.p_is_constant_object

   TT : T renames MyT;
   --% node.p_is_constant_object

   type My_Int_Array is array (1..3) of Integer;
   Tab : constant My_Int_Array := (2, 3, 5);

   Tab1 : Integer renames Tab (1);
   --% node.p_is_constant_object

   function R (A : Integer) return Integer;
end Renam;
