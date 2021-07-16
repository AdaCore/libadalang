procedure A is
   type Uint is range -2_099_999_999 .. -600_000_000;

   function "+" (Left : Uint; Right : Uint) return Uint with Import;

   type Array_Type is array (Positive range <>) of Integer;
   Arr : Array_Type (1 .. 1 + 2);
   pragma Test_Statement;
begin
   null;
end A;
