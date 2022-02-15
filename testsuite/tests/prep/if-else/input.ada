with Bar;

procedure Foo is
begin
#if X then
   Bar;
#else
   null;
#end if;
end Foo;
