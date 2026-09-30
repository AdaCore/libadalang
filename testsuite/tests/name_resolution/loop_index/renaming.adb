procedure Renaming is
   type Int_Array is array (Integer range <>) of Integer;

   A : constant Int_Array (1 .. 3) := (10, 20, 30);
begin
   for E of A loop
      declare
         R : Integer renames E;
      begin
         --  The prefix of Loop_Index must be the loop parameter itself, not a
         --  renaming of it.

         pragma Assert (R'Loop_Index = 1);
         pragma Test_Statement (Expect_Fail => True);
      end;
   end loop;
end Renaming;
