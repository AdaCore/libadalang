procedure Test is
   type R is record
      Y : Integer;
   end record;

   X : aliased R;
begin
   X'Unrestricted_Access.Y := 2;
   pragma Test_Statement;
end Test;
