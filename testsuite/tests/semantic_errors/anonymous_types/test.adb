procedure Test is
   type T is tagged null record;

   X : access Integer;
   Y : array (1 .. 10) of Integer;
   Z : access T'Class;
begin
   X := 2;
   pragma Test_Statement;

   Y := 3;
   pragma Test_Statement;

   Z := 4;
   pragma Test_Statement;
end Test;
