private with Pkg.Internal;
private package Pkg.Priv is
   Y : Integer := Pkg.Internal.X;
   pragma Test_Statement;
end Pkg.Priv;
