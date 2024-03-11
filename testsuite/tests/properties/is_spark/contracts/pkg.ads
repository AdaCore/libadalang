package Pkg with SPARK_Mode is
   function Foo (X : Integer) return Integer
      with Pre =>
        X >
        0  --% node.p_is_subject_to_proof
        ;

   function Bar (X : Integer) return Integer
      with Annotate => (GNATProve, Skip_Proof),
           Pre =>
              X >
              0  --% node.p_is_subject_to_proof
              ;
   --  The subprogram is annotated with `Skip_Proof` but this only applies to
   --  its body, hence the contract should still be subject to proof.
end Pkg;
