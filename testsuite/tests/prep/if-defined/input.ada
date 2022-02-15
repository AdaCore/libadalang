procedure Foo is
#if X'Defined then
   $X : constant String := "hello world";
#else
   I : constant Integer := -1;
#end if;
begin
   null;
end Foo;
