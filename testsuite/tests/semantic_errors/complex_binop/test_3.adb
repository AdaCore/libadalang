procedure Test is
   function Foo (X : Integer; Y : Boolean) return Integer is (X);
   function Foo (X : Integer; Y : Integer) return Boolean is (X);

   X : Boolean;
begin
   X := Foo (2 + 2, True);
   pragma Test_Statement;
end Test;

