with Pkg;

--  Instantiated with a statically constrained type: viewed from the
--  instantiation, the relation becomes a static expression.
package Inst is
   type Static_Index is range 1 .. 10;
   package My_Pkg is new Pkg (Static_Index);
   --% relop = node.p_designated_generic_decl.find(lal.RelationOp)
   --% relop.p_is_static_expr()
end Inst;
