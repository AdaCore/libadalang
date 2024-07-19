procedure Test is
   X : Integer;

   package Pkg is
      type T is tagged null record;

      function Foo (Self : T; X : Integer) return Integer is (0);
      function Foo (Self : T; X : Float) return Integer is (0);

      function Bar (Self : T) return Float is (0.0);
      function Bar (Self : T) return Boolean is (True);
   end Pkg;

   R : Pkg.T := (null record);
begin
   X := R.Foo (True);
   pragma Test_Statement (Expect_Fail => True);

   X := R.Bar;
   pragma Test_Statement (Expect_Fail => True);
end Test;
