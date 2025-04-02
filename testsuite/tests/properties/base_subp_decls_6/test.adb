procedure Test is
   package Pkg is
      type T is null record;
      type U is null record;

      procedure Foo (X : T; Y : U) is null;

      type T_D is new T;
      type U_D is new U;

      overriding
      procedure Foo (X : T_D; Y : U) is null;
      --% node.p_base_subp_declarations()

      overriding
      procedure Foo (X : T; Y : U_D) is null;
      --% node.p_base_subp_declarations()

      overriding
      procedure Foo (X : T_D; Y : U_D) is null;
      --% node.p_base_subp_declarations()
      --  Note: this case is not supported right now. While it's accepted by
      --  GNAT, it is not clear at this stage whether the overriding qualifier
      --  should be allowed (nor whether this primitive should exist at all!).
   end Pkg;
begin
   null;
end Test;
