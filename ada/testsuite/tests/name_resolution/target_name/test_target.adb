procedure Test_Target is
   A : Integer := 0;
begin
   A := @ + 1;
   pragma Test_Statement;
end Test_Target;
