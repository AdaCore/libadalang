with Bar;

procedure Foo is
begin
# if X then
   --  Comment a
   Bar (1);
   --  Comment b
# else
   --  Comment c
   Bar (2);
   --  Comment d
# end if;
end Foo;
