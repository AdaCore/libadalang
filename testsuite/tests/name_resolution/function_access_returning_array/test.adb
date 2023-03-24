procedure Test is
   type Arr is array (Positive range <>) of Integer;

   type F_Acc is access function (X : Positive) return Arr;

   function Foo (F : F_Acc) return Integer is
   begin
      return F (2) (1);
      pragma Test_Statement;
   end Foo;

   function Bar
     (F : access function (X : Positive) return Arr) return Integer
   is
   begin
      return F (2) (1);
      pragma Test_Statement;
   end Bar;
begin
   null;
end Test;
