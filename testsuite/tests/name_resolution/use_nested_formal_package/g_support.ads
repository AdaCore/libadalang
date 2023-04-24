with G_Solver;
with G_Solver_Ifc;
with G_Vars;

generic
package G_Support is
   package Vars is new G_Vars;
   package Ifc is new G_Solver_Ifc (Vars);
   package Solver is new G_Solver (Ifc);
   --% solver = node.p_designated_generic_decl
   --% obj = solver.find(lal.ObjectDecl)
   --% obj.f_type_expr.p_designated_type_decl
end G_Support;
