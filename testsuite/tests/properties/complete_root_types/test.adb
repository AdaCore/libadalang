--  Ensure that the internal package "root_types_" does not appear in the list
--  of completion returned by ``p_complete``.
procedure Test is
begin
   R;
   --% [x.decl for x in node.f_call.p_complete if x.decl.is_a(lal.PackageDecl)]
end Test;
