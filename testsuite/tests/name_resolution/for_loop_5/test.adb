with Ada.Containers.Vectors;

procedure Test is

   package Pkg is

      type JSON_Array is private with
        Iterable => (First       => Array_First,
                     Next        => Array_Next,
                     Has_Element => Array_Has_Element,
                     Element     => Array_Element);

      function Array_First (Arr : JSON_Array) return Positive;
      function Array_Next (Arr : JSON_Array; Index : Positive) return Positive;
      function Array_Has_Element
        (Arr : JSON_Array; Index : Positive) return Boolean;
      function Array_Element
        (Arr : JSON_Array; Index : Positive) return Integer;

   private

      package Vect_Pkg is new Ada.Containers.Vectors
        (Index_Type   => Positive,
         Element_Type => Integer);

      type JSON_Array is record
         Vals : Vect_Pkg.Vector;
      end record;

      function F (A : Json_Array) return Boolean;

   end Pkg;

   package body Pkg is

      function Array_First (Arr : JSON_Array) return Positive is
      begin
         return 1;
      end Array_First;
      function Array_Next (Arr : JSON_Array; Index : Positive) return Positive is
      begin
         return 1;
      end Array_Next;
      function Array_Has_Element
        (Arr : JSON_Array; Index : Positive) return Boolean is
      begin
         return True;
      end Array_Has_Element;
      function Array_Element
        (Arr : JSON_Array; Index : Positive) return Integer is
      begin
         return 0;
      end Array_Element;

      function F (A : Json_Array) return Boolean is
      begin
         for I in A loop
            null;
         end loop;
         pragma Test_Block;
         return True;
      end F;

   end Pkg;
begin
   null;
end Test;
