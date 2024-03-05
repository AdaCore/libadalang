package body Pkg with SPARK_Mode is
   procedure Test is
      procedure Inner_1;
      procedure Inner_2 with Annotate => (GNATProve, Skip_Proof);

      procedure Inner_1 is
         X : Integer;
      begin
         X :=
            2
            --% node.p_has_spark_mode_on
            --% node.p_is_subject_to_proof
            ;
      end Inner_1;

      procedure Inner_2 is
         X : Integer;
      begin
         X :=
            2
            --% node.p_has_spark_mode_on
            --% node.p_is_subject_to_proof
            ;
      end Inner_2;
   begin
      null;
      --% node.p_has_spark_mode_on
   end Test;
   --% node.p_has_spark_mode_on

begin
   Z :=
     4 --% node.p_has_spark_mode_on
     ;
end Pkg;
