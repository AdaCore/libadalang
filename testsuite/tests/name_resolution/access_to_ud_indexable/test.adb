procedure Test is
   package Vectors is
      subtype Element_Type is Integer;

      type Object is tagged limited private
         with Variable_Indexing => Index;
      type Ref_Element (Elm_Access : not null access Element_Type) is
         null record
         with Implicit_Dereference => Elm_Access;

      function Index (O : Object'Class; I : Positive) return Ref_Element
      is ((Elm_Access => new Element_Type));

   private
      type Object is tagged limited null record;
   end Vectors;

   type T_Vectors is access Vectors.Object;
   type T_Array is array (Positive range <>) of T_Vectors;
   type P_Array is access T_Array;

   A : T_Vectors;
   B : P_Array;
   I : Integer;
begin
   I := A (2);
   pragma Test_Statement;
   I := B (1) (2);
   pragma Test_Statement;
end Test;
