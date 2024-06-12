package Pkg.Sub is
   procedure P (X : T);
   --% type_expr=node.find(lal.TypeExpr)
   --% designated_type=type_expr.p_designated_type_decl
   --% referenced_type=type_expr.p_type_name.p_referenced_decl()
   -- designated_type should be equal to referenced_type
   --% designated_type is referenced_type
end Pkg.Sub;
