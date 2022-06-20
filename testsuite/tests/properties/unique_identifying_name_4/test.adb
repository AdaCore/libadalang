procedure Test is

   type My_C is ('a', 'é');

   --  The following node.f_defaut_expr.p_referenced_decl()s are real
   --  declarations.

   M_A : My_C := 'a';
   --% node.f_default_expr.p_referenced_decl().p_defining_name.p_unique_identifying_name
   --% node.f_default_expr.p_referenced_decl().p_subp_spec_or_null().p_returns

   M_E : My_C := 'é';
   --% node.f_default_expr.p_referenced_decl().p_defining_name.p_unique_identifying_name
   --% node.f_default_expr.p_referenced_decl().p_subp_spec_or_null().p_returns

   AA : Wide_Character := 'A';
   --% node.f_default_expr.p_referenced_decl().p_defining_name.p_unique_identifying_name
   --% node.f_default_expr.p_referenced_decl().p_subp_spec_or_null().p_returns

   --  The following node.f_defaut_expr.p_referenced_decl()s are synthetic
   --  nodes.

   A : Wide_Character := 'a';
   --% node.f_default_expr.p_referenced_decl().p_defining_name.p_unique_identifying_name
   --% node.f_default_expr.p_referenced_decl().p_subp_spec_or_null().p_returns

   E : Character := 'é';
   --% node.f_default_expr.p_referenced_decl().p_defining_name.p_unique_identifying_name
   --% node.f_default_expr.p_referenced_decl().p_subp_spec_or_null().p_returns

   EM : Wide_Wide_Character := '!';
   --% node.f_default_expr.p_referenced_decl().p_defining_name.p_unique_identifying_name
   --% node.f_default_expr.p_referenced_decl().p_subp_spec_or_null().p_returns

begin
   null;
end Test;
