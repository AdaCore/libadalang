private package Pkg.Bar is
   function Test return Integer;

   Y : Integer := Foo.X;
   pragma Test_Statement;
end Pkg.Bar;
