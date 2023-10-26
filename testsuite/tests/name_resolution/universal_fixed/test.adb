-- This test aims to use the predefined universal_fixed operators as specified
-- in ARM 4.5.5 - 18.
procedure Test is
     type T1 is delta 2.0**(-4) range -100.0 .. 100.0;
     type T2 is delta 2.0**(-5) range -100.0 .. 101.0;

     X1 : T1 := 6.0;
     X2 : T2 := 2.0;
     X3 : T1;
begin
     X3 := X1 / X1;  -- This is the operation defined on T1
     pragma Test_Statement;

     X3 := X1 / X2;  -- This is the operation defined on universal_fixed
     pragma Test_Statement;

     X3 := T1 (X1 / X1);  -- This is the operation defined on T1
     pragma Test_Statement;

     X3 := T1 (X1 / X2);  -- This is the operation defined on universal_fixed
     pragma Test_Statement;
end Test;
