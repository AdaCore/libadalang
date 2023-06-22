package body G is
   task body T with SPARK_Mode => Off is
      procedure P is null;
      -- Off because T is Off
   begin
      null;
   end T;

   protected body P is
      pragma SPARK_Mode (Off);
      function F return Boolean is
      begin
         return True;
      end F;
      -- Off because P is Off
      procedure Q is null;  -- Off because P is Off
      procedure R is null;  -- Off because P is Off
   end P;
end G;
