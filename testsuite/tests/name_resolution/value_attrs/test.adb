procedure Test is
   A : Integer := Integer'Value ("123");
   pragma Test_Statement;

   B : Integer := Integer'Wide_Value ("123");
   pragma Test_Statement;

   C : Integer := Integer'Wide_Wide_Value ("123");
   pragma Test_Statement;
begin
   null;
end Test;
