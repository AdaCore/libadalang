with Pkg.Child;

procedure Foo is
   I : constant Integer := Pkg.Child.Fact (6);
   pragma Test_Statement;
begin
   null;
end Foo;
