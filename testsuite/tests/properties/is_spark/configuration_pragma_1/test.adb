package body Test is
   procedure P is null;  -- On because of the configuration pragma
   procedure Q is null;  -- On because of the configuration pragma
   procedure R is null;  -- Off because of the annotation
   procedure S is null;  -- Off because of the annotation
   procedure T is null with SPARK_Mode => Off;  -- Off
   procedure U is null with SPARK_Mode => On;  -- On
end Test;
