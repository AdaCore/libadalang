procedure Test is
   type T is (A, B);
   subtype U is T;

   procedure Test1 is
      procedure T (X : T) is null;
   begin
      T (A);
      pragma Test_Statement;
   end Test1;

   procedure Test2 is
      function T (X : T) return T is (X);

      Y : U := T (A);
      pragma Test_Statement;
   begin
      null;
   end Test2;
begin
   null;
end Test;
