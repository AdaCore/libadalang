with System;

procedure Test is
   X : System.Address := System'To_Address (0);
   pragma Test_Statement_UID;
begin
   null;
end Test;
