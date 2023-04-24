--  style: non-ascii

procedure Test is

   type My_C is ('a', 'é');

   --  The following node.f_defaut_expr.p_referenced_decl()s are real
   --  declarations.

   M_A : My_C := 'a';
   --% decl = node.f_default_expr.p_referenced_decl()
   --% decl.p_defining_name.p_unique_identifying_name
   --% decl.p_subp_spec_or_null().p_returns

   M_E : My_C := 'é';
   --% decl = node.f_default_expr.p_referenced_decl()
   --% decl.p_defining_name.p_unique_identifying_name
   --% decl.p_subp_spec_or_null().p_returns

   AA : Wide_Character := 'A';
   --% decl = node.f_default_expr.p_referenced_decl()
   --% decl.p_defining_name.p_unique_identifying_name
   --% decl.p_subp_spec_or_null().p_returns

   --  The following node.f_defaut_expr.p_referenced_decl()s are synthetic
   --  nodes.

   A : Wide_Character := 'a';
   --% decl = node.f_default_expr.p_referenced_decl()
   --% decl.p_defining_name.p_unique_identifying_name
   --% decl.p_subp_spec_or_null().p_returns

   E : Character := 'é';
   --% decl = node.f_default_expr.p_referenced_decl()
   --% decl.p_defining_name.p_unique_identifying_name
   --% decl.p_subp_spec_or_null().p_returns

   EM : Wide_Wide_Character := '!';
   --% decl = node.f_default_expr.p_referenced_decl()
   --% decl.p_defining_name.p_unique_identifying_name
   --% decl.p_subp_spec_or_null().p_returns

begin
   null;
end Test;
