import libadalang as lal


ctx = lal.AnalysisContext()
u = ctx.get_from_file("test.adb")

type_decls = u.root.findall(lal.BaseTypeDecl)
for t1 in type_decls:
    for t2 in type_decls:
        if t1.p_is_derived_type(t2):
            print("{} derives from {}".format(
                t1.p_defining_name.text,
                t2.p_defining_name.text
            ))
