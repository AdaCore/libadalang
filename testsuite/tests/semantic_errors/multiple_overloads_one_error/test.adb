procedure Test is
   X : Integer;

   function Foo (X : Integer) return Boolean is (True);
   function Foo (X : Boolean) return Integer is (0);

   function Baz (X : Integer) return Boolean is (True);
   function Baz (X : Boolean) return Float is (0.0);

   function Bar (X, Y : Integer) return Integer is (X + Y);
begin
   X := Bar (Foo (True), Foo (2));
   pragma Test_Statement (Expect_Fail => True);

   X := Bar (Foo (True), Baz (2));
   pragma Test_Statement (Expect_Fail => True);
end Test;
