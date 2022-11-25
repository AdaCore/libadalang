procedure Test is
   type Array_Type is array (Integer range <>) of Integer;

   Arr : Array_Type (1 .. 10);
   I : Integer;
begin
   I := Array_Type (Arr) (I);
   pragma Test_Statement;
end Test;

