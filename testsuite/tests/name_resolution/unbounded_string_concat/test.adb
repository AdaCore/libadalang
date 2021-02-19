with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Test is
   X, Y : Unbounded_String;
begin
   X := X & Head (Y, 1);
   pragma Test_Statement_UID;
end Test;
