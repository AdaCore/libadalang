procedure Test is
   type Arr is array (Positive range <>) of Integer;

   A : Arr := (1 .. 10 => 0);
   B : Boolean := True;
begin
   A (2) := B;
   pragma Test_Statement;

   B := A (2);
   pragma Test_Statement;

   A (B) := 2;
   pragma Test_Statement;
end Test;
