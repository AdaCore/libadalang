package body Test is
   procedure P is null;  --  On because of the configuration pragma
                         --  Also illegal (body is on, spec is off)
end Test;
