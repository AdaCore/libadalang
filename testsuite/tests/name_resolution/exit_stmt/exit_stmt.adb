procedure Exit_Stmt is
   function Foo return Boolean;
   function Foo return Integer;

   function Bar return Integer;
begin
   loop
      pragma Section ("Correct case");
      exit when Foo;
      pragma Test_Statement;

      pragma Section ("Incorrect case");
      exit when Bar;
      pragma Test_Statement (Expect_Fail => True);
   end loop;

   pragma Section ("Exit with fully qualified name");
   L : Loop
      exit Exit_Stmt.L;
      pragma Test_Statement;
   end loop L;
end Exit_Stmt;
