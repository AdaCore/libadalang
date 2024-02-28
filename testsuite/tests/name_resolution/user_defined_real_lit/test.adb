procedure Test is
   package Big_Reals is
      type Big_Real is private
         with Real_Literal => To_Big_Real;
      pragma Test_Block;

      function To_Big_Real (X : String) return Big_Real;
      function To_Big_Real (X, Y : String) return Big_Real;

   private
      type Big_Real is record
         V : Float;  --  just kidding
      end record;
   end Big_Reals;

   package body Big_Reals is
      function To_Big_Real (X : String) return Big_Real is
      begin
         return (V => Float'Value (X));
      end To_Big_Real;
      function To_Big_Real (X, Y : String) return Big_Real is
      begin
         return (V => Float'Value (X));
      end To_Big_Real;
   end Big_Reals;

   X : Big_Reals.Big_Real := 2.2;
   pragma Test_Statement;

   N : constant := 3.3;
   Y : Big_Reals.Big_Real := N;
   pragma Test_Statement;

   subtype ST is Big_Reals.Big_Real;
   Z : ST := 4.4;
   pragma Test_Statement;
begin
   null;
end Test;
