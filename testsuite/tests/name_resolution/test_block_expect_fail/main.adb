procedure Main is
   procedure Nested is
      A : Integre;
   begin
   end Nested;
   pragma Test_Block (Expect_Fail => True);
begin
end Main;
