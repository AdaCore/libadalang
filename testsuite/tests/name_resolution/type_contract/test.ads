package Test is
   type A is private
      with Type_Invariant => F (A);
   pragma Test_Block;

   type B is record
      X : Integer;
   end record
      with Predicate => G (B.X);
   pragma Test_Block;

   type C is new Integer
      with Static_Predicate => C = 2;
   pragma Test_Block;

   type D is record
      X : Integer;
   end record
      with Dynamic_Predicate => G (D.X);
   pragma Test_Block;

   function F (X : A) return Boolean;
   function G (X : Integer) return Boolean is (True);
private
   type A is record
      X : Integer;
   end record;

   function F (X : A) return Boolean is (True);
end Test;
