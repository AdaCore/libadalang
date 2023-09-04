package G is
   task T with SPARK_Mode => On is
   private
      pragma SPARK_Mode (On);
   end T;

   protected P with SPARK_Mode => On is
      function F return Boolean;
      procedure Q;
   private
      pragma SPARK_Mode (On);
      procedure R;
   end P;
end G;
