package body Test is
   procedure P is null;
   -- There is no applicable configuration pragma, so the body is not analyzed
   -- (same as Off).
   procedure Q is null;
   -- There is no applicable configuration pragma, so the body is not analyzed
   -- (same as Off).
   procedure R is null;
   -- There is no applicable configuration pragma, so the body is not analyzed
   -- (same as Off).
   procedure S is null;
   -- There is no applicable configuration pragma, so the body is not analyzed
   -- (same as Off).

   procedure T with SPARK_Mode => Off is
      procedure P;

      procedure P is null with SPARK_Mode => Off;
      -- Off
   begin
      null;
   end T;
   -- Off

   procedure U with SPARK_Mode => Off is
   begin
      null;
   end U;
   -- On
end Test;
