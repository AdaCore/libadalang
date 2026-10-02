procedure Test_At is
   X : Integer := 1;
   Y : Integer;
begin
   <<L1>>
   X := X + 1;
   Y := X'At (L1);
end Test_At;
pragma Test_Block;
