pragma SPARK_Mode (On);

package Test is
   pragma Elaborate_Body;
private
   function F1 return Boolean is (True) with SPARK_Mode => Off;

   function F2 return Boolean is (True) with SPARK_Mode => On;

   function F3 return Boolean is (True);
   pragma Annotate (GNATprove, Skip_Proof, F3);
end Test;
