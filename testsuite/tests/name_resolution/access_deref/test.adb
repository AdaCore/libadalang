procedure Test is
   A : aliased Integer;
begin
   A := A'Access.all;
   pragma Test_Statement;

   A := A'Unchecked_Access.all;
   pragma Test_Statement;

   A := A'Unrestricted_Access.all;
   pragma Test_Statement;
end Test;
