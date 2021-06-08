procedure Test is
   type T is new Natural range 0 .. 10;

   Y : T'Base := 3;
   X : T'Base := T'Base (12) + 1;
   pragma Test_Statement;
begin
   null;
end Test;
