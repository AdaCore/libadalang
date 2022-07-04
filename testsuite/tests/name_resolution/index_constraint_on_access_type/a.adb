procedure A is
   type Arr is array (Integer range <>) of Integer;
   type Arr_Access is access Arr;

   X : Arr_Access (1 .. 4);
   pragma Test_Statement;
begin
   null;
end A;
