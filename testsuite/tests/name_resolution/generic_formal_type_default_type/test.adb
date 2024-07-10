procedure Test is
   generic
      type T is range <> or use Integer;
   package Pkg is
      X : T;
      --% node.f_type_expr.p_designated_type_decl
   end Pkg;

   package My_Pkg_1 is new Pkg;
   --% obj = node.p_designated_generic_decl.find(lal.ObjectDecl)
   --% obj.f_type_expr.p_designated_type_decl

   package My_Pkg_2 is new Pkg (Long_Integer);
   --% obj = node.p_designated_generic_decl.find(lal.ObjectDecl)
   --% obj.f_type_expr.p_designated_type_decl
begin
   null;
end Test;
