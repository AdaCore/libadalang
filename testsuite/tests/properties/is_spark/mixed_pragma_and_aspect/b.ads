package B with SPARK_Mode => On is
   procedure P;
private
   pragma SPARK_Mode (Off);
   procedure Q;
end B;
