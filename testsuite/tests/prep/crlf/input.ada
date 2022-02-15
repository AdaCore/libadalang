procedure Foo is
   --  Comment: B : Boolean := False;
begin
#if X then
   Bar (1);
#end if;
end Foo;
