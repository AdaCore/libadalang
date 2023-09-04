package body F with SPARK_Mode is

   procedure P with SPARK_Mode => On is
      procedure Q with SPARK_Mode => Off;

      procedure Q is
         pragma SPARK_Mode (Off);
      begin
         null;
      end Q;
      -- Off

      procedure R;

      procedure R is null;
      -- On because P is On
   begin
      null;
   end P;
   -- On
end F;
