function Foo (B : Boolean) return Integer is
   R : Integer;
begin
   if B then
      R := 1;
   else
      R := 0;
   end if;
   return R;
   pragma Test_Statement;
end Foo;
