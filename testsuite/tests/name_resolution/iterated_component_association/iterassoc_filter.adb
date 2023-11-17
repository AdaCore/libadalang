procedure Iterassoc_Filter is
   type R is range 1 .. 5;
   type A is array (R) of Float;

   X : A := (1.0, 2.0, 3.0, 4.0, 5.0);

   M : A := (for I in R when X (I) >= 3.0 => X (I) * X (I));

   O : A := (for F : Float of X when F >= 3.0 => F * F);

   P : A := (for F of X when F >= 3.0 => F * F);


   type AA is array (R, R) of Float;

   Q : AA :=
     (for I in R when X (I) >= 3.0 =>
       (for J in R when X (J) >= 3.0 => X (I) * X (J)));

   R : Boolean := (for all F of X when F >= 3.0 => F > 4.0);

   S : Boolean := (for some I in 1 .. 5 when X (I) >= 3.0 => X (I) > 4.0);
begin
   null;
end Iterassoc_Filter;
pragma Test_Block;
--  We use Test_Block so that inner ForLoopSpec and ForLoopIterFilter nodes
--  are also resolved.
