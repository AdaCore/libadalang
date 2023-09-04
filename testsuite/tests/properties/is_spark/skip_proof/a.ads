pragma SPARK_Mode;

package A is
   procedure P with Annotate => (GNATprove, Skip_Proof);

   procedure Q with Annotate => (GNATprove, Skip_Proof);

   procedure R;
   pragma Annotate (GNATprove, Skip_Proof, R);

   procedure V;
   pragma Annotate (Bruuu);
   pragma Annotate (GNATprove, Skip_Proof, V);
   pragma Annotate (GNATprove, Skip_Flow_And_Proof, V);
   pragma Annotate (Gruuu);


   procedure S;

   procedure T;

   procedure U;

   procedure W
     with Annotate => (Grrr),
          Annotate => (Brrrrr),
          Annotate => (GNATprove, False_Positive,
                       "message to be justified", "reason"),
          Annotate => (GNATprove, Skip_Proof),
          Annotate => (GNATprove, Skip_Flow_And_Proof);

   procedure X
     with Annotate => (Grrr),
          Annotate => (Brrrrr),
          Annotate => (GNATprove, Skip_Proof);
   pragma Annotate (GNATprove, Skip_Flow_And_Proof, X);
   pragma Annotate (GNATprove, False_Positive,
                    "message to be justified", "reason");
end A;
