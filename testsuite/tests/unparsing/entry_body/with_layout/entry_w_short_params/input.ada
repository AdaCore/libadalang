entry Get (V : out Integer) when Is_Set is
begin
   --  Entry is blocked until the
   --  condition is true. The barrier
   --  is evaluated at call of entries
   --  and at exits of procedures and
   --  entries. The calling task sleeps
   --  until the barrier is released.

   V := Local;
   Is_Set := False;
end Get;
