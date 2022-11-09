procedure Test is
   type T is tagged null record;

   External_Tag : String := T'External_Tag;
   pragma Test_Statement;
begin
   null;
end Test;
