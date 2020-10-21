procedure Test_It is
   type T is range 1 .. 100;
   type NT is new Test_It.T'Base;
   type Arr is array (1 .. 10) of Integer;
   type Arr_2 is array (Integer range <>) of Integer;
   type Arr_3 is array (Integer range 1 .. 10) of Integer;

   Var : Integer := 10;
   type Arr_4 is array (Integer range 1 .. Var) of Integer;
begin
   null;
end Test_It;
