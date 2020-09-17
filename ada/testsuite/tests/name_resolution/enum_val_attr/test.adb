procedure Test is
   type E is (A, B, C);

   X : E := E'Enum_Val (1);
   pragma Test_Statement;
begin
   null;
end Test;
