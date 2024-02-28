procedure Test is
   package Big_Ints is
      type Big_Int is private
         with Integer_Literal => To_Big_Int;
      pragma Test_Block;

      function To_Big_Int (X : Boolean) return Big_Int;
      function To_Big_Int (X : String) return Big_Int;
      function To_Big_Int (X, Y : String) return Big_Int;
      function To_Big_Int (X : String) return Integer;

   private
      type Big_Int is record
         V : Integer;  --  just kidding
      end record;
   end Big_Ints;

   package body Big_Ints is
      function To_Big_Int (X : String) return Big_Int is
      begin
         return (V => Integer'Value (X));
      end To_Big_Int;
   end Big_Ints;

   X : Big_Ints.Big_Int := 2;
   pragma Test_Statement;

   N : constant := 3;
   Y : Big_Ints.Big_Int := N;
   pragma Test_Statement;

   subtype Big_Pos is Big_Ints.Big_Int;
   Z : Big_Pos := 4;
   pragma Test_Statement;

   XX : Big_Ints.Big_Int := X;
   pragma Test_Statement;
begin
   null;
end Test;
