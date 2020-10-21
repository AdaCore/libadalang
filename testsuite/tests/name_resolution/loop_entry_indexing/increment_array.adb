procedure Increment_Array is
   type Integer_Array is array (Positive range <>) of Integer;
   procedure Internal (X : in out Integer_Array) is
   begin
      for J in X'Range loop
         pragma Loop_Invariant (X'Loop_Entry (12) = 15);
         pragma Test_Statement;
      end loop;
   end Internal;
begin
   null;
end;
