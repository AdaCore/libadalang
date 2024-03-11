package body Pkg.Child_Private_Part with SPARK_Mode => Off is
   procedure Test is
   begin
      null;
      --% node.p_has_spark_mode_on
   end Test;
begin
   X :=
      1  --% node.p_has_spark_mode_on
      ;
end Pkg.Child_Private_Part;

