procedure Test is
   type T is null record;
   type A is array (Positive range <>) of T;
   type U is access constant A;

   Y : U;
   X : A := Y (1 .. 2)'Unrestricted_Access.all;
   pragma Test_Statement;
begin
   null;
end Test;
