with Pkg;
package Pkg.Bar is
   type U is new Pkg.T;
   X : constant U := Foo;
   pragma Test_Statement;
end Pkg.Bar;
