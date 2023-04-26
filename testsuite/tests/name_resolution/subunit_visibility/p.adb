with Types; use Types;

procedure P is
   type Before is new Integer;
   procedure Subunit is separate;
   type After is new String;
begin
   Subunit;
end P;
