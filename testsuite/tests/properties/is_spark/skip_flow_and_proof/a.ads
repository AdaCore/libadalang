pragma SPARK_Mode (On);

package A is
   procedure P with Annotate => (GNATprove, Skip_Flow_And_Proof);

   procedure Q with Annotate => (GNATprove, Skip_Flow_And_Proof);

   procedure R;
   pragma Annotate (GNATprove, Skip_Flow_And_Proof, R);

   procedure S;

   procedure T;

   procedure U;
end A;
