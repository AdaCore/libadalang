procedure Test is
   generic
      type T is private;
   package Vectors is
      type Elements_Array is array (Positive range <>) of T;
      type Vector is record
         EA : Elements_Array (1 .. 10);
      end record;

      procedure Swap (V : in out Vector; I, J : Positive);
   end Vectors;

   package body Vectors is
      procedure Swap (V : in out Vector; I, J : Positive) is
      begin
         V.EA (I) := V.EA (J);
      end Swap;
   end Vectors;

   package Int_Vectors is new Vectors (Integer);
   package Int_Vectors_Vectors is new Vectors (Int_Vectors.Vector);
   pragma Test_Statement;
begin
   null;
end Test;
