--  Test that a reference to a generic inside the generic itself returns an
--  entity with the proper rebindings on it.
procedure Test is
   generic
   procedure P2_G;

   procedure P2_G is
   begin
      P2_G;
   end;

   procedure P2 is new P2_G;
   --% gen_body = node.p_designated_generic_decl.p_body_part()
   --% gen_body.find(lal.CallStmt)[0].p_referenced_decl().entity_repr
begin
   null;
end Test;
