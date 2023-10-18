procedure Test is
   type Fix_Delta is delta 0.5 range -3.0 .. 3.0;
   subtype Sub_Delta is Fix_Delta delta 1.0 range 0.0 .. 2.0;
   pragma Test_Statement;

   type Fix_Digits is digits 5 range -50.0 .. 50.0;
   subtype Sub_Digits is Fix_Digits digits 1 range 0.0 .. 30.0;
   pragma Test_Statement;
begin
   null;
end Test;
