procedure Test is
   type List is array (Character range <>) of Integer;

   subtype List_A  is List ('A' .. 'A');
   pragma Test_Statement;

   subtype List_E  is List ('E' .. 'E');
   pragma Test_Statement;

   subtype List_AE is List ('A' .. 'E');
   pragma Test_Statement;
begin
   null;
end Test;
