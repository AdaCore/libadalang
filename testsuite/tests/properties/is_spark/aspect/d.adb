package body D is
   procedure P1 is null;
   -- Off, because this section and the previous ones are Off
   procedure P2 is separate;
   procedure P3 is null;
   -- Off, because this section and the previous ones are Off
end D;
