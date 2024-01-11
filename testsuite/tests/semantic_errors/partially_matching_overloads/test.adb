procedure Test is
   X : Integer;

   function Foo (X : Integer) return Boolean is (True);
   function Foo (X : Boolean) return Integer is (0);
begin
   X := Foo (2);
   pragma Test_Statement;
end Test;
