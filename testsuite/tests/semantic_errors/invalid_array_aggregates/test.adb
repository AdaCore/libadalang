procedure Test is
   type Arr is array (Positive range <>) of Integer;

   A : Arr := (1 .. 10 => True);
   pragma Test_Statement;

   B : Arr := (True .. False => 2);
   pragma Test_Statement;
begin
   null;
end Test;
