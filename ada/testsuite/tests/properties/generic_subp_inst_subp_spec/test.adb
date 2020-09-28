procedure T925_008 is
   generic
      type TP is private;
   procedure G(T : TP);

   procedure Test is
      procedure G_Instance is new G(TP => Integer);
   begin
      G_Instance(42);
      --% $node[0].p_referenced_decl().p_subp_spec_or_null()
      --% $node[0].p_is_dispatching_call()
      --  ^
      --  is_dispatching_call uses subp_spec_or_null.
   end Test;

   procedure G(T : TP) is
   begin
      null;
   end G;
begin
   null;
end T925_008;
