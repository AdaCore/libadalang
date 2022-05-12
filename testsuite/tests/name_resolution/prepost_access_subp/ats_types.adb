procedure ats_types is
   type T1 is access procedure (X : Integer) with Pre => X mod 2 = 0;
   pragma Test_Block;
   type T2 is access procedure (X : Integer) with Post => X mod 2 = 0;
   pragma Test_Block;

   -- NOTE: nameres gives a wrong result for My_Size in the Pre aspect below.
   -- It should refer to My_Size from the parameter specification but it refers
   -- to the number declaration instead. See TN V513-007.

   My_Size : constant := 64;
   type An_Acc_Sub is access procedure (My_Size : Natural)
   with Size => My_Size, Pre => My_Size mod 64 = 0;
   pragma Test_Block;
begin
   null;
end ats_types;
