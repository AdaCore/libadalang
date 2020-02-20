procedure Test is
   type Arr is array (Integer range 1 .. 10) of Integer;

   X : Arr := (others => 0);
begin
   X (1 + 1 .. 3) := X (2 + 2 .. 5);
   pragma Test_Statement;
end Test;
