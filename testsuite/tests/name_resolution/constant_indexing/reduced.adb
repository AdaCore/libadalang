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
      with Constant_Indexing => Get;

      function Get (Object : JSON_Value; Key : String) return JSON_Value;

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
   end Types;

   package body Types is

      function "=" (Left : String; Right : JSON_Value) return Boolean is
        (if Right.Kind = Integer_Kind then
            True
         else
            False);
      pragma Test_Statement;

      function Get (Object : JSON_Value; Key : String) return JSON_Value is
      begin
         return Object;
      end Get;

   end Types;
begin
   null;
end Reduced;
