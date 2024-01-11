procedure Test is
   type Subp is access function Foo (X : Integer) return Integer;

   function Foo (F : Subp; X : Integer) return Boolean is
   begin
      X := F (True);
      pragma Test_Statement;

      return F (X);
      pragma Test_Statement;
   end Foo;
begin
   null;
end Test;
