--  Test that 'Enabled resolves correctly, not failing because we cannot find
--  the prefix. It will work both with correct GNAT checks names, and
--  incorrect/non existing ones.

procedure Test is
begin
   if Container_Checks'Enabled then
      null;
   end if;
   pragma Test_Statement;

   if Non_Existing_Check'Enabled then
      null;
   end if;
   pragma Test_Statement;
end Test;
