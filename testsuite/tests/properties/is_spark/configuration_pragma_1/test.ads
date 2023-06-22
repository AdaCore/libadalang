package Test is
   procedure P;

   procedure Q with SPARK_Mode => On;

   procedure R with Annotate => (GNATprove, Skip_Proof);

   procedure S with Annotate => (GNATprove, Skip_Flow_And_Proof);

   procedure T;
   procedure U;
end Test;
