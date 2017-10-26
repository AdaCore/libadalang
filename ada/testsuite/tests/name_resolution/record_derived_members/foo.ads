--  Test that ame resolution for record members can resolve the inherited
--  members in derived record types.

package Foo is
   type R1_Type is record
      A, B : Integer;
      C    : Natural;
   end record;

   type R2_Type is new R1_Type;

   type R3_Type is tagged record
      D : Integer;
   end record;

   type R4_Type is new R3_Type with null record;

   type R5_Type is new R4_Type with record
      E : Natural;
   end record;

   R2 : R2_Type;

   R3 : R3_Type;
   R4 : R4_Type;
   R5 : R5_Type;

   R5_Class : R5_Type'Class := R5;

   pragma Test (R2.A);
   pragma Test (R2.Z);

   pragma Test (R4.D);
   pragma Test (R4.Z);

   pragma Test (R5.D);
   pragma Test (R5.E);
   pragma Test (R5.Z);

   pragma Test (R5_Class.D);

end Foo;
