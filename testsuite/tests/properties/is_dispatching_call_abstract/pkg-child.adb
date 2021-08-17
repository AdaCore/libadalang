package body Pkg.Child is
   procedure Main (X : access U) is
   begin
      Foo (X);
      --% node.f_call.p_is_dispatching_call()
   end Main;

   procedure Foo (X : access U) is
   begin
      null;
   end Foo;
end Pkg.Child;
