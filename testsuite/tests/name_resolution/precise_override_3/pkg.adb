package body Pkg is
   procedure Bar is
      X : U;
   begin
      Foo (X);
      pragma Test_Statement;
   end Bar;
end Pkg;
