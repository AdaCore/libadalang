with G_Solver_Ifc;

generic
   with package Ifc is new G_Solver_Ifc (<>);
package G_Solver is
   use Ifc;
   use Vars;

   X : Logic_Var;
end G_Solver;

