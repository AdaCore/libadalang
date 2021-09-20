procedure Main is
   package Foo is
      type T is null record;

      function "=" (X, Y : T) return Boolean is (True);
      pragma Find_All_References (Any);
   end Foo;

   use Foo;

   A, B : T;
begin
   if A /= B then
      null;
   end if;
   if "/=" (A, B) then
      null;
   end if;
end Main;
