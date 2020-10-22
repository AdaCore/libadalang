procedure Test is
   type T is delta 0.1 range 0.0 .. 10.0;

   A : constant := T'Delta;
   pragma Test_Statement;
begin
   null;
end Test;
