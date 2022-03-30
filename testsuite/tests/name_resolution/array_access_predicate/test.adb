procedure Test is
   type T is array (Positive range <>) of Integer;
begin
   declare
      type U is access all T
         with Predicate => U'First = 1;
   begin
      null;
   end;
   pragma Test_Block;
end Test;
