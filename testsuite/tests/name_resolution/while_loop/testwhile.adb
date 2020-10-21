procedure Testwhile is
   function Foo return Integer;
   function Foo return Boolean;

   I : Integer;
begin
   while Foo loop
      I := Foo;
      pragma Test_Statement;
      exit when Foo;
      pragma Test_Statement;
   end loop;
   pragma Test_Statement;
end Testwhile;
