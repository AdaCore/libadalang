procedure Test_Array_Fn is
   type Integer is range 0 .. 1000;
   type T is array (Integer range 1 .. 10) of Integer;
   function F return T is (others => 10);

   A : Integer;
begin
   A := F (8);
   pragma Test_Statement;
end Test_Array_Fn;
