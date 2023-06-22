package body E is
   procedure P1 with SPARK_Mode => On is
   begin
      null;
   end P1;
   -- On
   procedure P2 is separate;
   procedure P3 with SPARK_Mode => Off is
   begin
      null;
   end P3;
   -- Off
end E;
