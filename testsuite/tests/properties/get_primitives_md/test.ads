package Test is
   type T_Pre is tagged null record;
   --% f_prim=node.p_get_primitives()[0]
   --% v_param=f_prim.p_subp_spec_or_null().p_params[0].p_defining_name

   function F (V : T_Pre) return Integer is (0)
      with Pre'Class => Other_F (V);
   --% v_ref=node.findall(lal.Identifier)[-1]
   --% v_decl=v_ref.p_referenced_defining_name()

   function Other_F (V : T_Pre) return Boolean is (False);

   -- check that the parameter retrieved through `get_primitives` does not
   -- carry metadata that would make it incompatible with that same parameter
   -- retrieved through the reference to V.
   --% (v_param == v_decl)
end Test;

