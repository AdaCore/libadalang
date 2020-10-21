package body Pkg is
   procedure Foo  (X : Boolean) is null;
   --% node.p_decl_part(imprecise_fallback=True)
   function Bar return Integer is ((42));
   --% node.p_decl_part(imprecise_fallback=True)
end Pkg;
