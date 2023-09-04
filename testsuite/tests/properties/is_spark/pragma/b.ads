package B is
   pragma SPARK_Mode (On);
   procedure P;
private
   pragma SPARK_Mode (Off);
   procedure Q;
end B;
