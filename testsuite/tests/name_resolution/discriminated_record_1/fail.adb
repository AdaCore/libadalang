procedure Fail is
   type Enum is (A, B, C);

   type Fail_Rec (E : Enum) is record
      C1, C2 : Integer;
   end record;

   F : Fail_Rec := (A, 12, 15);
   pragma Test_Statement;
begin
   null;
end Fail;
