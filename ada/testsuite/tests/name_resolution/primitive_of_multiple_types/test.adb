with Pkg;
with Pkg2;

procedure Test is
   X : Pkg2.T;
   Y : Pkg.A;
begin
   X := Pkg2.Foo (Y);
   pragma Test_Statement;
end Test;
