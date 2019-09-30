procedure Test_Binop_Res is
   package P is
      type A is private;
   private
      type A is range 1 .. 20;
   end P;

   package body P is
      C : A := 12;
      B : A := 12 * C;
      pragma Test_Statement;
   end P;
begin
   null;
end Test_Binop_Res;
