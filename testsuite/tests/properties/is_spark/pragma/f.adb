package body F is
   pragma SPARK_Mode;

   procedure P is
      pragma SPARK_Mode (On);

      procedure Q;  -- Off because of the pragma
      pragma SPARK_Mode (Off);

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
