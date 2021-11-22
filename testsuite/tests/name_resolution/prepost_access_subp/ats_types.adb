procedure ats_types is
   type T1 is access procedure (X : Integer) with Pre => X mod 2 = 0;
   pragma Test_Block;
   type T2 is access procedure (X : Integer) with Post => X mod 2 = 0;
   pragma Test_Block;
begin
   null;
end ats_types;