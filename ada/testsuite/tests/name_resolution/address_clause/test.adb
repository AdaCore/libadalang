procedure Test is
   X, Y : Integer;
   for Y'Address use X'Address;
   pragma Test_Statement;
begin
   null;
end Test;
