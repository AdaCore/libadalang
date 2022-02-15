with Bar;

procedure Foo is
begin
#if X then
   Bar (1);
#elsif Y then
   Bar (2);
#else
   Bar (3);
#end if;
end Foo;
