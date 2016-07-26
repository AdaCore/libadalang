with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type Integer is range 0 .. 10000;
   type Int_Array is array (Integer range 1 .. 10) of Integer;
   type Int_Array_Array is array (Integer range 1 .. 10) of Int_Array;

   A : Int_Array_Array := (others => (others => 5));
begin
   pragma Test (A (5));
   pragma Test (A (5) (5));
   pragma Test (A (5, 6) (5));
   pragma Test (A (5) (5) (5));
end Test;
