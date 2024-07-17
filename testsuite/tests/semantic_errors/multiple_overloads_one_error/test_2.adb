procedure Test_2 is
   X : Integer;

   --  same as Test but order is reversed, meaning the first argument of
   --  the call to bar will need to resolve the overload in two steps.
   --  We don't want that to pollute the diagnostics.
   function Foo (X : Boolean) return Integer is (0);
   function Foo (X : Integer) return Boolean is (True);

   function Bar (X, Y : Integer) return Integer is (X + Y);
begin
   X := Bar (Foo (True), Foo (2));
   pragma Test_Statement (Expect_Fail => True);
end Test_2;
