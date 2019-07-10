--  Check that array index resolution gracefully fails when an expression is
--  ambiguous in such a way that an expression could either be an array indice
--  of rank > 1, or a function parameter.

procedure Testar
is
   type Array_Type is array (Positive range <>) of Integer;

   function Arr (A, B : Positive := 12) return Array_Type
   is (Array_Type'(1 .. A => B));

   C : Array_Type := Arr (12, 15);
   pragma Test_Statement;
begin
   null;
end Testar;
