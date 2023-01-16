private with Pkg.Foo.Bar;
with B.Foo;

package Pkg.Test is
   use B;

   package My_Gen is new Foo.Gen;
   pragma Test_Statement;
end Pkg.Test;
