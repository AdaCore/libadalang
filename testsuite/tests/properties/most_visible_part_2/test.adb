with Pkg_G;

procedure Test is
   package My_Pkg is new Pkg_G;
begin
   My_Pkg.Foo (My_Pkg.X);
   --% fun = node.f_call.f_name.p_referenced_decl()
   --% typ_t = fun.findall(lal.Identifier)[2].p_referenced_decl()
   --% complete_t = typ_t.p_most_visible_part(fun.p_body_part())
   --% typ_u = complete_t.next_sibling
   --% complete_u = typ_u.p_most_visible_part(fun.p_body_part())
end Test;
