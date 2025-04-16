procedure Test is
   package Pkg is
      type T is null record;

      procedure Foo (X : T) is null;
   end Pkg;

   package Pkg_2 is
      type T_2 is new Pkg.T;

      overriding
      procedure Foo (X : T_2) is null;
      --% node.p_base_subp_declarations()
      --% node.p_root_subp_declarations()

      procedure Bar (X : T_2) is null;
      --% node.p_base_subp_declarations()
      --% node.p_root_subp_declarations()
   end Pkg_2;

   package Pkg_3 is
      type T_3 is new Pkg_2.T_2;

      overriding
      procedure Foo (X : T_3) is null;
      --% node.p_base_subp_declarations()
      --% node.p_root_subp_declarations()

      overriding
      procedure Bar (X : T_3) is null;
      --% node.p_base_subp_declarations()
      --% node.p_root_subp_declarations()
   end Pkg_3;
begin
   null;
end Test;

