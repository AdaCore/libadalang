procedure Test is
   X : constant := 1 + 2;
   pragma Test_Statement;

   X_2 : Integer := 1 + 2;
   pragma Test_Statement;

   Y : constant := -1;
   pragma Test_Statement;

   Y_2 : Integer := -1;
   pragma Test_Statement;
begin
   null;
end Test;
