procedure Return_Stmt is
   function Foo return Integer is
      function Bar return Integer is (12);
      function Bar return Float is (12.0);
   begin
      return Bar;
      pragma Test_Statement;
   end Foo;
begin
   null;
end Return_Stmt;
