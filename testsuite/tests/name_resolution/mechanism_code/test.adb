procedure Test is
   procedure Pro (A, B, C : Integer) is null;

   I : Integer;
begin
   I := Pro'Mechanism_Code;
   pragma Test_Statement;
   I := Pro'Mechanism_Code (1);
   pragma Test_Statement;
   I := Pro'Mechanism_Code (3);
   pragma Test_Statement;
end;
