package body D is
   procedure P1 is null;  -- Auto -> Off because it's the default
   procedure P2 is separate;
   procedure P3 is null;  -- Auto -> Off because it's the default
end D;
