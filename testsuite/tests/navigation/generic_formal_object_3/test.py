import libadalang as lal

ctx = lal.AnalysisContext()
u = ctx.get_from_file("test.adb")

inst = u.root.find(lal.GenericPackageInstantiation)
decl = inst.p_designated_generic_decl.f_package_decl
objs = decl.findall(lal.ObjectDecl)
for obj in objs:
    anon_expr_decl = obj.f_default_expr.p_referenced_decl()
    print("Formal designated by {} is {} with value {}".format(
        obj.p_defining_name.text,
        anon_expr_decl.p_get_formal().text,
        anon_expr_decl.f_expr.text
    ))
