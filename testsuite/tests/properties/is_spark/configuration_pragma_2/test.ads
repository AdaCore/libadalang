package Test is
   procedure P;

   procedure Q with SPARK_Mode => Off;

   procedure R with SPARK_Mode => On;

   procedure S with Annotate => (GNATprove, Skip_Proof);

   procedure T with Annotate => (GNATprove, Skip_Flow_And_Proof);
end Test;
