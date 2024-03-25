with Ada;
--% node.p_has_spark_mode_on

package Pkg with SPARK_Mode is
   pragma Elaborate_Body;

   X : constant Integer :=
     2 --% node.p_has_spark_mode_on
     ;

   procedure Test;
private
   Z : Integer :=
     1 --% node.p_has_spark_mode_on
     ;
end Pkg;
