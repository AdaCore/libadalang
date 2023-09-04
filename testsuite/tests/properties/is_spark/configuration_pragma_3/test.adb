pragma SPARK_Mode (Off);

package body Test is
   procedure P is null;  -- Off because of the configuration pragma above
   procedure Q is null;  -- Off because of the configuration pragma above
   procedure R is null;  -- Off because of the configuration pragma above
   procedure S is null;  -- Off because of the configuration pragma above
   procedure T is null with SPARK_Mode => Off;  -- Off
end Test;
