procedure Main is
   package Foo is
      type T is null record;
      --% neq = node.p_get_primitives()[1]
      --% neq.p_defining_name.p_find_all_references([neq.unit])

      function "=" (X, Y : T) return Boolean is (True);
   end Foo;

   use Foo;

   A, B : T;
begin
   if A /= B then
      null;
   end if;
end Main;
