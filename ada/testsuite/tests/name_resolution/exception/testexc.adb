procedure Testexc is
   A, B : exception;
   pragma Test_Statement;

   C : exception renames A;
   pragma Test_Statement;
begin
   raise A;
   pragma Test_Statement;
end Testexc;
