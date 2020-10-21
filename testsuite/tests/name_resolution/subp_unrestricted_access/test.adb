procedure Test is
   type W_Char is access procedure (C : Character);
   type W_Eol is access procedure;

   procedure Print_Char (C : Character);
   procedure Print_Eol;

   A : W_Char := Print_Char'Unrestricted_Access;
   pragma Test_Statement;
   B : W_Eol  := Print_Eol'Unrestricted_Access;
   pragma Test_Statement;
begin
   null;
end Test;
