procedure Test is
   function "-" (X : Integer) return Integer renames Standard."+";

   function "+" (X : Integer) return Integer is
   begin
      return X;
   end "+";

   function "+" (X : Float) return Float is
   begin
      return X;
   end "+";

   B : Boolean;
begin
   B := (+2) **  (-2) <  "-"  (-1);     -- 2**2 < 1
   pragma Test_Statement;
end Test;
