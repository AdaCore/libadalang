procedure Foo (I : Int) is
   Foo : exception;
begin
   if I = 0 then
      raise Foo;
   end if;
end Foo;
