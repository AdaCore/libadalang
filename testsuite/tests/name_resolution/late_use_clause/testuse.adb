procedure Testuse is

   type Float is digits 6 range 0.0 .. 20.0;
   type Integer is range 0 .. 20;

   function Foo return Integer is (12);

   package B is
      function Foo return Float is (12.0);
   end B;

begin
   declare
      C : Float := Foo;
      pragma Test_Statement (Expect_Fail => True);
      use B;
   begin
      null;
   end;
end Testuse;
