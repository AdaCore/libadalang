package G is
   task T with SPARK_Mode => On is
   private
   end T;

   protected P with SPARK_Mode => On is
      function F return Boolean;
      procedure Q;
   private
      procedure R;
   end P;
end G;
