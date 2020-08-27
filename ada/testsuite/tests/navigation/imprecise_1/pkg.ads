package Pkg is
   procedure Foo  (X : Integer);
   --% node.p_body_part(imprecise_fallback=True)
   procedure Bar;
   --% node.p_body_part(imprecise_fallback=True)
end Pkg;
