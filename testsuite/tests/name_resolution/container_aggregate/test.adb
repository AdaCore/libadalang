pragma Ada_2022;

procedure Test is
   package JSON is
      type JSON_Value is private
        with Integer_Literal => To_JSON_Value;

      function To_JSON_Value (Text : String) return JSON_Value;

      type JSON_Array is private
        with Aggregate => (Empty       => New_JSON_Array,
                           Add_Unnamed => Append);
      pragma Test_Block;

      function New_JSON_Array return JSON_Array;

      procedure Append
        (Self  : in out JSON_Array;
         Value : JSON_Value) is null;

   private
      type JSON_Value is null record;
      type JSON_Array is null record;

      function To_JSON_Value (Text : String) return JSON_Value
        is (null record);

      function New_JSON_Array return JSON_Array is (null record);
   end JSON;
begin
   null;
end Test;
