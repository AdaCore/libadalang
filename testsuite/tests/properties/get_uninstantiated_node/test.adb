procedure Test is
   generic
   package Pkg is
      type T is null record;
   end Pkg;

   package Pkg_Inst is new Pkg;

   X : Pkg_Inst.T;
   --% node.f_type_expr.p_designated_type_decl.p_fully_qualified_name
   --% node.f_type_expr.p_designated_type_decl.p_get_uninstantiated_node.p_fully_qualified_name
begin
   null;
end Test;
