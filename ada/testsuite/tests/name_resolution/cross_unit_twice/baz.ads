package Baz is
   Loul : Integer;

   type Record_Type_2 is record
      B : Integer;
   end record;

   type Record_Type is record
      A : Record_Type_2;
   end record;
end Baz;
