import libadalang as lal


ctx = lal.AnalysisContext()
u = ctx.get_from_file('pkg.ads')
enum_subp_spec = u.root.find(lal.ObjectDecl).f_default_expr.p_called_subp_spec
print(enum_subp_spec.p_parent_basic_decl)
