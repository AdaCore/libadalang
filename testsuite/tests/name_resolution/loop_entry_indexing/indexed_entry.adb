procedure Indexed_Entry is
   type T is record
      Max : Integer;
   end record;
   type Cell_Array is array (Integer range 1 .. 10) of T;

   A : Cell_Array;
   I : Integer;
begin
   loop
      pragma Loop_Invariant (I = A'Loop_Entry (I).Max);
      pragma Test_Statement;
   end loop;
end;
