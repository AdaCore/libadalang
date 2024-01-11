procedure Test is
   function Foo (X : Boolean) return Boolean is (X);

   R : Integer;
begin
   R := (case Foo (3) is
      when True => 1,
      when False => 2);
   pragma Test_Statement;
end Test;
