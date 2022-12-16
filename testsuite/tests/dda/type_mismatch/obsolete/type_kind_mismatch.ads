package Type_Kind_Mismatch is

   type T1 is record
      I : Integer;
   end record;

   type T2_Parent is mod 2 ** 8;
   type T2_Child is null record;

end Type_Kind_Mismatch;
