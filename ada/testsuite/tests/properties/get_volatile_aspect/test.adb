procedure Test is
   type VolatileType is new Integer;
   pragma Volatile (VolatileType);

   VolatileVariable : Integer;
   pragma Volatile (VolatileVariable);
begin
   null;
end Test;
