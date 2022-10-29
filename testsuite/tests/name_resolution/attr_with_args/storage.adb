function Storage (A, B : String) return Boolean is
begin
   return A'Overlaps_Storage (B) and A'Has_Same_Storage (B);
end Storage;
pragma Test_Block;
