package D is
   procedure P1;
   pragma SPARK_Mode;

   procedure P2;
   pragma SPARK_Mode (On);

   procedure P3;
end D;
