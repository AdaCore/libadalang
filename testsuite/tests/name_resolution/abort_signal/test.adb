with Ada.Exceptions; use Ada.Exceptions;

procedure Test is
   X : Exception_Id := Standard'Abort_Signal'Identity;
   pragma Test_Statement;
begin
   raise Standard'Abort_Signal;
   pragma Test_Statement;
end Test;
