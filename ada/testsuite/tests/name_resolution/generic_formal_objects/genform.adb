procedure Genform is
   type R is record
      B : Integer;
   end record;

   I : R := (B => 12);

   generic
      A : R;
   package GF1 is
   end GF1;

   generic
      type T is private;
      A : T;
   package GF2 is
   end GF2;

   package Inst1 is new GF1 (A => I);
   pragma Test_Statement;

   package Inst2 is new GF2 (T => R, A => (B => 12));
   pragma Test_Statement;
begin
   null;
end Genform;
