package body Test is
   procedure P is null;  -- Auto -> Off because it's the default
   procedure Q is null;  -- Auto -> Off because it's the default
   procedure R is null;  -- Auto -> Off because it's the default
   procedure S is null;  -- Off because of the annotation
   procedure T is null;  -- Off because of the annotation
end Test;
