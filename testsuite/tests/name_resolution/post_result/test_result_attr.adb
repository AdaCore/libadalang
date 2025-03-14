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

   type T4 is array (Integer range 1 .. 10) of T3;

   function Id (X : T4) return T4
   is (X)
   with Post => Id'Result (1).A = 8;

   type T5 is new T4 (1 .. 5);
   function Id (X : T5) return T5;
   pragma Post (
      (declare
         I : constant Integer := 8;
       begin
       Id'Result (1).A = I)
   );
begin
   null;
end Test_Result_Attr;
pragma Test_Block;
