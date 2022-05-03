pragma Cfg_3;

separate (Pkg)
procedure Foo is
   procedure Bar;
   procedure Bar is separate;
begin
   null;
end Foo;
