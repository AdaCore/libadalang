procedure Test is
   X : Integer;
   Y : Integer;
   for X use at Y'Address;
   pragma Test_Statement;
begin
   null;
end Test;
