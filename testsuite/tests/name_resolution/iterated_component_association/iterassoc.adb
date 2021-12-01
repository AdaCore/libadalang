procedure Iterassoc is
   type R is range 1 .. 5;
   type A is array (R) of Float;

   X : A := (1.0, 2.0, 3.0, 4.0, 5.0);

   M : A := (for I in R => X (I) * X (I));
   pragma Test_Statement;

   M : A := (for I : R in 1 .. 2 => X (I) * X (I));
   pragma Test_Statement;

   N : A := (for I in 1 .. 5 => X (I) * X (I));
   pragma Test_Statement;

   O : A := (for F : Float of X => F * F);
   pragma Test_Statement;

   P : A := (for F of X => F * F);
   pragma Test_Statement;


   type AA is array (R, R) of Float;

   Q : AA :=
     (for I in 1 .. 5 =>
       (for J in 1 .. 5 => X (I) * X (J)));
   pragma Test_Statement;
begin
   null;
end Iterassoc;
