procedure Test is
   type Arr is array (Positive range <>) of Integer;

   Need_Bounds : Boolean with Import;

   X : Integer;

   Bnd_Args : Arr (1 .. (if Need_Bounds then X else 0));
   pragma Test_Statement;
begin
   null;
end Test;

