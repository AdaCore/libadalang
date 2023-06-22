package C with SPARK_Mode is
   procedure P;
private
   pragma SPARK_Mode (Off);
   procedure Q;
end C;
