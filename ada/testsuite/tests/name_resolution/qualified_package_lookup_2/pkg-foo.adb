procedure Pkg.Foo is
   X : Pkg.T;
   Y : Integer;
begin
   Y := X.X;
   pragma Test_Statement;
end Pkg.Foo;
