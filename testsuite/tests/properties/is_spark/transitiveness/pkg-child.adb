package body Pkg.Child with SPARK_Mode is
   procedure Foo is
      X : constant Integer :=
         2  --% node.p_has_spark_mode_on
         ;
   begin
      declare
         K : constant Integer :=
            X --% node.p_has_spark_mode_on
            ;
      begin
         null;
      end;
   end Foo;
   --% node.p_is_subject_to_proof

   procedure Bar is
      X : constant Integer :=
         2  --% node.p_is_subject_to_proof
         ;
   begin
      declare
         K : constant Integer :=
            X  --% node.p_is_subject_to_proof
            ;

         procedure Inner;

         procedure Inner is
         begin
            null;
            --% node.p_is_subject_to_proof
         end Inner;
      begin
         null;
      end;
   end Bar;
   --% node.p_is_subject_to_proof

   procedure Baz is separate;
   --% node.p_is_subject_to_proof
   --
   procedure Bazz is separate;
   --% node.p_is_subject_to_proof

begin
   pragma SPARK_Mode (Off);

   K :=
     0 --% node.p_has_spark_mode_on
     ;
end Pkg.Child;

