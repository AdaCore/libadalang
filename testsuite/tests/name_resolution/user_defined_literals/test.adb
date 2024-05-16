with Ada.Numerics.Big_Numbers.Big_Reals;
use  Ada.Numerics.Big_Numbers.Big_Reals;

procedure Test is
   procedure P (A : Big_Real) with
      Pre => A > Big_Real'(0.0);
   pragma Test_Block;

   procedure P (A : Big_Real) is null;

   function F (A : Big_Real) return Boolean is
   begin
      return A > Big_Real'(0.0);
   end F;
   pragma Test_Block;
begin
   null;
end Test;
