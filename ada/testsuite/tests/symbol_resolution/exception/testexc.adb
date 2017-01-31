procedure Testexc is
   A, B : exception;
begin
   raise A;
   pragma Test_Statement;
end Testexc;
