procedure Test is
   N : constant := (if Long_Long_Float'Machine_Mantissa = 2 then 1 else 2);
   pragma Test_Statement;
   P : constant := N * 2;
   pragma Test_Statement;

   F : constant := (if True then 1.0 else 2.0);
   pragma Test_Statement;
   Q : constant := F * 2;
   pragma Test_Statement;
begin
   null;
end Test;
