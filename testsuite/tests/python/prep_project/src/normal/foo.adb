with Bar;

procedure Foo is
   I : constant Integer := $FOO;
   S : constant String :=
#if FOOBAR'Defined then
   "FOOBAR is defined"
#else
   "FOOBAR is *not* defined"
#end if;
end Foo;
