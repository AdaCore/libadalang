procedure Foo is
   --  Comment:
begin
#if X then
   Bar (1);
#end if;
end Foo;