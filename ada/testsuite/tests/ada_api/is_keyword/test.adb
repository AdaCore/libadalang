procedure Test is
   protected type A is
      I : Float := 0.1;
   end A;

   type A is synchronized interface;

   overriding function Bar is null;

   I : access Float := A.I'Access;
   B : Boolean := (for some I in 1 .. 10 => I mod 2 = 0);
begin
   null;
end Test;
