pragma Extensions_Allowed (All);
--  Test in this file won't work until libadalang!1031 is fixed

package body Test is
begin
   procedure P1 with SPARK_Mode is
      procedure P2 with SPARK_Mode => Off is
      begin
         null;
      end P2;
   begin
      null;
   end P1;
end Test;
