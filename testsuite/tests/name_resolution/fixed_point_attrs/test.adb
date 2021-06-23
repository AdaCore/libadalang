procedure Test is
   type T is delta 0.1 range 0.0 .. 10.0;

   A : constant := T'Delta;
   pragma Test_Statement;

   B : constant T := T'Fixed_Value (3);
   pragma Test_Statement;

   C : constant Integer := Integer'Integer_Value (B);
   pragma Test_Statement;
begin
   null;
end Test;
