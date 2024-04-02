package Test with SPARK_Mode => Off is
   procedure P; -- Off because of enclosing `SPARK_Mode => Off`
end Test;
