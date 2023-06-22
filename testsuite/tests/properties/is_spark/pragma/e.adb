package body E is
   procedure P1 is
      pragma SPARK_Mode (On);
   begin
      null;
   end P1;
   -- On
   procedure P2 is separate;
   procedure P3 is
      pragma SPARK_Mode (Off);
   begin
      null;
   end P3;
   -- Off
   procedure P4 is
   begin
      null;
   end P4;
   -- Off
end E;
