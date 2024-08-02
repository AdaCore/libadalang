procedure Test_Result_Attr is
   function Barize return Integer
      with Post => Barize'Result = 12;

   function Barize return Integer is
   begin
      return 12;
   end Barize;

   type T1 is access function (A : Boolean) return Boolean with
      Post => T1'Result = A;

   type T2 is access function (A : Boolean) return Boolean;

   pragma Post (T2'Result = True);

   type T3 is record A : Integer; end record;

   function Expr return T3 is (A => 8) with
      Post => Expr'Result.A = 8;
begin
   null;
end Test_Result_Attr;
pragma Test_Block;
