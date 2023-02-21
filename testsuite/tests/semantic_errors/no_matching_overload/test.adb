procedure Test is
   X : Integer;

   function Foo (X : Integer) return Integer is (0);
   function Foo (X : Float) return Integer is (0);
begin
   X := Foo (True);
   pragma Test_Statement;
end Test;
