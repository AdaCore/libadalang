procedure Test_2 is
   X : Integer;

   function Foo (X : Integer) return Boolean is (True);
   function Foo (X : Integer) return Integer is (0);

   function Bar (X : Float) return Integer is (0);
   function Bar (X : Duration) return Integer is (0);
begin
   X := Bar (Foo (2));
   pragma Test_Statement (Expect_Fail => True);
end Test_2;
