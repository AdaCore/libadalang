procedure Test is
   package Pkg is
      type T is tagged null record;
   end Pkg;

   package Der is
      type U is new Pkg.T with null record;
      --% prims = node.p_get_primitives(include_predefined_operators=True)
      --% eq_op = prims[0]
      --% eq_op.p_base_subp_declarations()
   end Der;
begin
   null;
end Test;
