package Pkg.Child_Private_Part with SPARK_Mode is
   procedure Test;
private
   pragma SPARK_Mode (Off);

   X : Integer :=
     3 --% node.p_has_spark_mode_on
     ;

end Pkg.Child_Private_Part;
