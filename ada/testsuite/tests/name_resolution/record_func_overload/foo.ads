--  Test that name resolution for record members work when the prefix
--  designates ambiguously one record type or another one because of overloaded
--  functions.

package Foo is

   type R1_Type is record
      A : Integer;
   end record;

   type R2_Type is record
      B : Integer;
   end record;

   function F (I : Integer) return R1_Type is ((A => I));
   function F (I : Integer) return R2_Type is ((B => I * 2));

   pragma Test (F (1).A);
   pragma Test (F (1).B);
   pragma Test (F (1).Z);

   type R3_Type is record
      C : R1_Type;
   end record;

   type R4_Type is record
      C : R2_Type;
   end record;

   function G (I : Integer) return R3_Type is ((C => F (I)));
   function G (I : Integer) return R4_Type is ((C => F (I)));

   pragma Test (G (1).C.A);
   pragma Test (G (1).C.B);
   pragma Test (G (1).C.Z);

end Foo;
