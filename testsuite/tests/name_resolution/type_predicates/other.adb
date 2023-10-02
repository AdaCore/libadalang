procedure Other is
   type Color is (White, Red, Yellow, Green, Blue, Brown, Black)
      with Static_Predicate => Color in Red;
   pragma Test_Block;

   type Column is range 1 .. 72
      with Predicate => Column mod 2 = 1;
   pragma Test_Block;

   type Table is array (1 .. 10) of Integer;
   pragma Predicate (Table, Table'First = 1);
   pragma Test_Statement;

   type R is record
      I : Integer;
   end record with Predicate => R.I = 1;
   pragma Test_Block;
begin
   null;
end Other;
