with Pkg;

procedure Main is
   use type Pkg.R;
   X : Pkg.R;
begin
   X := X + X;
   pragma Test_Statement;
end Main;
