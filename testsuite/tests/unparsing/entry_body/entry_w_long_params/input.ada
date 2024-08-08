entry Get (V : out Integer; T : out Integer; R : Integer; W : Integer)
  when Is_Set
is
   X : constantInteger := R + W;
begin
   --  Entry is blocked until the
   --  condition is true. The barrier
   --  is evaluated at call of entries
   --  and at exits of procedures and
   --  entries. The calling task sleeps
   --  until the barrier is released.

   V := Local + X;
   T := Local + X;
   Is_Set := False;
end Get;
