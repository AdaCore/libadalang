package body Pkg.Bar is
   function Test return Integer is
   begin
      return Foo.X;
      pragma Test_Statement;
   end Test;
end Pkg.Bar;
