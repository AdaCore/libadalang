--  This testcase is derived from the json library:
--  https://github.com/onox/json-ada.

procedure Reduced is
   generic
      type Integer_Type is range <>;
      type Float_Type is digits <>;

   package Types is

      type Value_Kind is
        (Integer_Kind,
         Float_Kind);

      type JSON_Value (Kind : Value_Kind) is tagged private
      with
        Constant_Indexing => Get_C,
        Variable_Indexing => Get_V;
      pragma Test_Block;

      type Json_Value_Const_Ref
        (V : not null access constant JSON_Value) is private
      with
        Implicit_Dereference => V;

      type Json_Value_Ref (V : not null access JSON_Value) is private with
        Implicit_Dereference => V;

      function Get_V (Object : JSON_Value; Key : String) return Json_Value_Ref;
      function Get_C
        (Object : JSON_Value; Key : String) return Json_Value_Const_Ref;

   private

      subtype Array_Offset is Natural;

      type JSON_Value (Kind : Value_Kind) is tagged record
         case Kind is
            when Integer_Kind =>
               Integer_Value : Integer_Type;
            when Float_Kind =>
               Float_Value : Float_Type;
         end case;
      end record;

      type Json_Value_Const_Ref
        (V : not null access constant JSON_Value) is null record;

      type Json_Value_Ref (V : not null access JSON_Value) is null record;
   end Types;

   package body Types is

      function "=" (Left : String; Right : JSON_Value) return Boolean is
        (if Right.Kind = Integer_Kind then
            True
         else
            False);
      pragma Test_Statement;

      function Get_V
        (Object : JSON_Value; Key : String) return JSON_Value_Ref is
      begin
         return R : JSON_Value_Ref := (V => Object'Unrestricted_Access);
      end Get_V;

      function Get_C
        (Object : JSON_Value; Key : String) return JSON_Value_Const_Ref is
      begin
         return R : constant JSON_Value_Const_Ref :=
           (V => Object'Unrestricted_Access);
      end Get_C;

   end Types;
begin
   null;
end Reduced;
