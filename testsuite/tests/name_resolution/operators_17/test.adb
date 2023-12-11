procedure Test is
   type T is delta 0.1 range -10.0 .. 10.0;

   X : T := 2.0;
   B : Boolean;
   I : Integer := Integer (2 * X);
   pragma Test_Statement;
begin
   B := X * Integer'(2) = 4.0;
   pragma Test_Statement;
   B := Integer'(2) * X = 4.0;
   pragma Test_Statement;
end Test;
