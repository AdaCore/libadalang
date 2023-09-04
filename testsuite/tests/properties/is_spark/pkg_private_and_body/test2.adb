pragma Extensions_Allowed (All);

package body Test2 is
   pragma SPARK_Mode;
   function F10 return Boolean is (True);
   --% node.p_get_aspect("SPARK_Mode")
   --% node.p_is_spark
begin
   pragma SPARK_Mode (Off);
   function F20 return Boolean is (True);
end Test2;
