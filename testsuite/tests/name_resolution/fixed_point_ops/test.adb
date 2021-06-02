procedure Test is
   type T is delta 0.1 range -1.0 .. 1.0;
   X : T;
begin
   X := X / 12.1;
   pragma Test_Statement;

   X := X * 2;
   pragma Test_Statement;

   X := X * Integer'(2);
   pragma Test_Statement;

   X := 1.0 * Integer'(2);
   pragma Test_Statement;
end Test;
