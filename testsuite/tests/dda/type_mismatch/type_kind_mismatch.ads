package Type_Kind_Mismatch is

   type T1 is mod 2 ** 8;
   --  Placeholder.
   --  Placeholder.

   type T2_Parent is tagged null record;
   type T2_Child is new T2_Parent with null record;

end Type_Kind_Mismatch;
