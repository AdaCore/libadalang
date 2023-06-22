package C is
   pragma SPARK_Mode;
   procedure P;
private
   pragma SPARK_Mode (Off);
   procedure Q;
end C;
