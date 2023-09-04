package body B with SPARK_Mode => Off is
   procedure P is null; -- Off because B is Off
   procedure Q is null; -- Off because B is Off
begin
   pragma SPARK_Mode (Off);
   null;
end B;
