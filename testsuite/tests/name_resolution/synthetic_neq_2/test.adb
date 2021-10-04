procedure Test is
   package Foo is
      type T is null record;

      function "=" (A, B : T) return Boolean is (True);
   end Foo;

   package Bar is
      type U is new Foo.T;
   end Bar;

   use Bar;

   A, B : U;
begin
   if A /= B then
      raise Program_Error;
   end if;
   pragma Test_Statement;

   if "/=" (A, B) then
      raise Program_Error;
   end if;
   pragma Test_Statement;
end Test;
