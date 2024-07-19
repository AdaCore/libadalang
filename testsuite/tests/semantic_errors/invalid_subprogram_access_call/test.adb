procedure Test is
   type Subp is access function Foo (X : Integer) return Integer;

   function Foo (F : Subp; X : Integer) return Boolean is
   begin
      X := F (True);
      pragma Test_Statement (Expect_Fail => True);

      return F (X);
      pragma Test_Statement (Expect_Fail => True);
   end Foo;
begin
   null;
end Test;
