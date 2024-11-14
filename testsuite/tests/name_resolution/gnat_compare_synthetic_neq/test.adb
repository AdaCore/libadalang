procedure Test is
   package P is
      type T is null record;

      overriding
      function "=" (A, B : T) return Boolean;
   end P;
   package body P is
      overriding
      function "=" (A, B : T) return Boolean is (True);
   end P;

   use P;

   A, B : T;
begin
   if A /= B then
      null;
   end if;
end Test;

