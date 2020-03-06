package body Pkg.Impl_G is
   procedure Test is
      X : Integer := Impl_G.X;
      pragma Test_Statement;
   begin
      null;
   end Test;
end Pkg.Impl_G;
