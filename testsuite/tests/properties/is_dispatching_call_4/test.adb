procedure Test is
   package Abs_Pkg is
      type T is abstract tagged null record;

      procedure Foo (X : T) is null;
   end Abs_Pkg;

   package C_Pkg is
      type U is new Abs_Pkg.T with null record;

      overriding procedure Foo (X : U) is null;
   end C_Pkg;

   procedure Main is
      X : C_Pkg.U;

      use C_Pkg;
      use Abs_Pkg;
   begin
      Foo (X);
      --% node.f_call.p_is_dispatching_call()
   end Main;
begin
   Main;
end Test;
