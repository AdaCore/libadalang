package E is
   procedure P1;
   pragma SPARK_Mode;

   procedure P2;
   pragma SPARK_Mode (On);

   procedure P3;
   pragma SPARK_Mode;

   procedure P4;
   pragma SPARK_Mode;
end E;
