procedure Test is
   generic
      type T is abstract tagged private;
      with procedure Foo (X : T) is abstract;
   package Pkg is
      procedure Bar (X : T'Class);
   end Pkg;

   package body Pkg is
      procedure Bar (X : T'Class) is
      begin
         Foo (X);
         --% node.f_call.p_is_dispatching_call()
      end Bar;
   end Pkg;

   package T_Def is
      type T is abstract tagged null record;
      procedure Wow (X : T) is null;
   end T_Def;

   package Pkg_Inst is new Pkg (T_Def.T, T_Def.Wow);

   package U_Def is
      type U is new T_Def.T with null record;
      overriding procedure Wow (X : U) is null;
   end U_Def;

   X : U_Def.U;
begin
   Pkg_Inst.Bar (X);
end Test;
