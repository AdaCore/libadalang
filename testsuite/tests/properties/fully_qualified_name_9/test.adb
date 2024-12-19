procedure Test is
   X : access Integer;
   --% typ=node.f_type_expr.p_designated_type_decl
   --% typ.p_fully_qualified_name

   Y : array (1 .. 5) of Integer;
   --% typ=node.f_type_expr.p_designated_type_decl
   --% typ.p_fully_qualified_name
begin
   null;
end Test;
