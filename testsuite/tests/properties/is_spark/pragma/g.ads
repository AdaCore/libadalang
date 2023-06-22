package G is
   task T is
      pragma SPARK_Mode (On);
   private
      pragma SPARK_Mode (On);
   end T;

   protected P is
      pragma SPARK_Mode (On);
      function F return Boolean;
      procedure Q;
   private
      pragma SPARK_Mode (On);
      procedure R;
   end P;
end G;
