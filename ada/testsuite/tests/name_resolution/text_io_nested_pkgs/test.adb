with Ada.Text_IO;

procedure Test is
   type U is mod 10;
   package MIO is new Ada.Text_IO.Modular_IO (U);
   pragma Test_Statement_UID;
begin
   null;
end Test;
