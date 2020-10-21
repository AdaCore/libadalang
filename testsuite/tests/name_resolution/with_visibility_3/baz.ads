package Baz is
   type Integer is range 1 .. 2 ** 32;
   Loul : Integer;

   type Record_Type_2 is record
      B : Integer;
   end record;

   type Record_Type is record
      A : Record_Type_2;
   end record;

   function Pouet return Integer;
end Baz;
