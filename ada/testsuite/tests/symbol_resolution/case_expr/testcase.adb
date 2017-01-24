procedure Testcase is
   function Foo return Integer is (12);
   function Foo return Boolean is (True);

   A : Integer := 15;
   B : Float;
begin
   A := (case A is
         when 12 | 13 => A,
         when 15 | 17 => Foo);
   pragma Test_Statement;
end Testcase;
