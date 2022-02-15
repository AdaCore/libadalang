with Bar;

procedure Foo is
begin
# if X then
   Bar (1);
# else
   Bar (2);
# end if;
end Foo;
