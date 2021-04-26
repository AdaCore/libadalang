procedure Test is
   type T is mod 128;

   X : T := 12;
begin
   X := "and" (X, X);
   pragma Test_Statement;
end Test;
