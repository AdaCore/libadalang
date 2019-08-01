package Test is
   type A;

   type A is private;

private
   type A is record
      C, D : Integer;
   end record;

   type B;

   type C;

   type C is record
      X : Integer;
   end record;
end Test;
