with Bar;

procedure Foo is
begin
#if X then
   Bar (1);
#  if Y then
   Bar (2);
#  end if;
#else
   Bar (3);
#  if Z then
   Bar (4);
#  end if;
#end if;
end Foo;
