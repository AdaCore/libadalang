procedure Main is
   package Foo is
      type T is null record;

      function "=" (A, B : T) return Boolean is (True);
   end Foo;

   A, B : Foo.T;
begin
   if A /= B then
      raise Program_Error;
   end if;
   pragma Test_Statement;
end Main;
