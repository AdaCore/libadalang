procedure Test is
   type My_Integer is range - (2 ** 31) .. 2 ** 31 - 1;
   pragma Test_Statement;

   function "**" (L, R : My_Integer) return My_Integer;
begin
   null;
end Test;
