procedure Test is
   function Foo (X : Integer) return Integer is
      pragma Pre (X > 0);
      pragma Precondition (X < 10);
      pragma Post (Foo'Result > 0);
      pragma Postcondition (Foo'Result < X);
   begin
      return X - 1;
   end Foo;
   pragma Test_Block;
begin
   null;
end Test;
