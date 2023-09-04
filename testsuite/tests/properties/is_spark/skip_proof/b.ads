package B is
   pragma SPARK_Mode;
   procedure P with Annotate => (GNATprove, Skip_Proof);
   procedure Q;
end B;
