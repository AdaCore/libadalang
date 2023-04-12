package PK3_Bis is
   type Int_Array is array (Positive range <>) of Integer;
   type Record_Type is record
      A : Int_Array (1 .. 10);
   end record;
end PK3_Bis;
