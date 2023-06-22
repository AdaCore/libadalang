package Test2 with SPARK_Mode => On is
   pragma Elaborate_Body;
private
   pragma SPARK_Mode (On);

   function F1 return Boolean is (True);

   function F2 return Boolean is (True) with SPARK_Mode => Off;

   function F3 return Boolean is (True);
   pragma Annotate (GNATprove, Skip_Proof, F3);

   function F4 return Boolean is (True);
   pragma Annotate (GNATprove, Inline_For_Proof, F4);
   pragma SPARK_Mode (Off);
end Test2;
