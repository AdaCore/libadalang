procedure Test is
   function Foo (X : Integer) return Integer is (X);
   function Foo (X : Boolean) return Boolean is (X);

   procedure Bar (X : Integer) is null;
begin
   Bar (Foo (2.5));
   pragma Test_Statement;
end Test;
