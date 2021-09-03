package body Pkg.Child is
   procedure Main (X : access U) is
   begin
      Foo (X);
      pragma Test_Statement;
   end Main;

   procedure Foo (X : access U) is
   begin
      null;
   end Foo;
end Pkg.Child;
