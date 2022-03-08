with Bar;
with Pkg.Child;

procedure Foo is
   I : constant Integer := Pkg.Child.Fact (6);
   pragma Test_Statement;
begin
   Bar;
   pragma Test_Statement;
end Foo;
