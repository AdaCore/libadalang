package body G is
   task body T is
      pragma SPARK_Mode (Off);

      procedure P is null;  -- Off
   begin
      null;
   end T;

   protected body P is
      pragma SPARK_Mode (Off);
      function F return Boolean is
      begin
         return True;
      end F;
      -- Off
      procedure Q is null;  -- Off
      procedure R is null;  -- Off
   end P;
end G;
