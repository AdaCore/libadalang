procedure Testchrlit is
   type Char is ('a', 'b', 'c');

   C : Char;
begin
   C := 'a';
   pragma Test_Statement;

   C := 12;
   pragma Test_Statement;
end Testchrlit;
