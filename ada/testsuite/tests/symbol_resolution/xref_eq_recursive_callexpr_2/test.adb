with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type Integer is range 0 .. 10000;
   type Int_Array is array (Integer range 1 .. 10) of Integer;
   type Int_Array_Array is array (Integer range 1 .. 10) of Int_Array;

   A : Int_Array_Array := (others => (others => 5));

   function Pouet return Int_Array_Array is (A);

   C : Int_Array := (others => 5);

   B : Integer;
begin
   pragma Test (Pouet (5));
   pragma Test (Pouet (5) (5) (5));
   pragma Test (Pouet (5) (5));

   B := C (5);
   pragma Test_Statement;

   B := A (5) (8);
   pragma Test_Statement;

   B := Pouet (5) (8);
   pragma Test_Statement;

   B := Pouet (5) (8.0);
   pragma Test_Statement;
end Test;
