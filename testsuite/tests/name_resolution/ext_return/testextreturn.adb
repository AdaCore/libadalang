procedure Testextreturn is
   function Foo return Integer is
   begin
      return A : Integer do
         A := 12;
         pragma Test_Statement;
      end return;
      pragma Test_Statement;
   end Foo;
begin
   null;
end Testextreturn;
