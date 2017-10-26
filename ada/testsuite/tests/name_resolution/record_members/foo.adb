--  Test that name resolution for record members resolve to the member
--  declaration in simple cases.

package Foo is
   type Char is ('a', 'b', 'c');

   type String is array (Positive range <>) of Char;
   type Integer is range 1 .. 100;

   type R1_Type is record
      A, B : Integer;
      C    : Natural;
   end record;

   type R2_Type (N : Natural) is record
      I : Integer;
      S : String (1 .. N);
   end record;

   type R2_Array is array (Natural range <>) of R2_Type;

   subtype R3_Type is R2_Type (10);

   type R4_Type is record
      R1 : R1_Type;
      R2 : R2_Array (2);
   end record;

   R1_1 : R1_Type;
   function R1_2 return R1_Type is ((A => 1, B => 2, C => 3));
   function R1_3 (I : Integer) return R1_Type is
     ((A => I, B => I * 2, C => 0));

   R2_1 : R2_Type := (N => 1, others => <>);
   R2_2 : R2_Type (20);

   R3 : R3_Type;

   R4 : R4_Type;

   pragma Test (R1_1.A);
   pragma Test (R1_1.D);
   pragma Test (R1_2.A);
   pragma Test (R1_3 (1).A);

   pragma Test (R2_1.N);
   pragma Test (R2_1.S (1));
   pragma Test (R2_1.A);

   pragma Test (R3.N);
   pragma Test (R3.S (1));

   pragma Test (R4.R1.C);
   pragma Test (R4.R2 (1).I);

end Foo;
