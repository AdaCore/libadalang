procedure Test is
   type E is (A, B, C);

   V : Boolean := E'Valid_Value ("B");
   pragma Test_Statement;
begin
   null;
end Test;
