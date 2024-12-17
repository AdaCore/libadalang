with P0; use P0;
with P1; use P1;

package P2 is

   type R1_Child is new R1_Parent with record
      X1 : Integer;
   end record;

   type R2_Child is new R2_Parent with record
      X3 : Integer;
   end record;

   type R3_Child is new R3_Parent with record
      X2 : Integer;
   end record;
   pragma Test (0);
   pragma Test (1);
   pragma Test (2);
   pragma Test (3);
   pragma Test (10);

   type R4_Child is new R4_Parent with record
      X2 : Integer;
   end record;
   pragma Test (False);
   pragma Test (True);

   type R5_Child is new R5_Parent with record
      X1 : Integer;
   end record;

   type R6_Child is new R6_Parent;

   type R7_Child is new R7_Parent with record
      X2 : Integer;
   end record;

   type R8_Child is new R8_Parent with record
      X2 : Integer;
   end record;

end P2;
