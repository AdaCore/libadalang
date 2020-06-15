procedure Test is
   type Enum_Lit is (A, B, C, D, E, F);
   type Chr is new Character range 'G' .. 'Z';
   type Str is array (A .. F) of Chr;
   type Chr_Grid is array (D .. F, 1 .. 3) of Chr;

   procedure Proc (X : Str) is null;
   procedure Proc (X : Chr_Grid) is null;
begin
   Proc ("STRING");
   pragma Test_Statement;
end Test;
