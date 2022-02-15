with Bar;

procedure Foo is
begin
   # if X'Defined then
      Bar ($X);
   # else
      Bar;
   # end if;
end Foo;
