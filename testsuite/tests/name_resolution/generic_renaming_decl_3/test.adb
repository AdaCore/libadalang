with Test_2;

procedure Test is
   generic function Gen renames Test_2;

   function Equals is new Gen (Integer);
   pragma Test_Statement;
begin
   null;
end Test;
