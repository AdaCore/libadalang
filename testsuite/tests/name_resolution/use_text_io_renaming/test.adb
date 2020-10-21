with Ada.Integer_Wide_Text_IO; use Ada.Integer_Wide_Text_IO;

procedure Test is
   X : Integer;
   Last : Positive;
begin
   Get ("12", X, Last);
   pragma Test_Statement_UID;
end Test;
