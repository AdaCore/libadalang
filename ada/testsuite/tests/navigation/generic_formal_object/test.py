import libadalang as lal


def format_object_decl(decl):
    if decl.f_renaming_clause is None:
        return "<AnonymousObjectDecl {} = {}>".format(
            decl.f_type_expr.text,
            decl.f_default_expr.text
        )
    else:
        return "<AnonymousObjectDecl {} renames {}>".format(
            decl.f_type_expr.text,
            decl.f_renaming_clause.f_renamed_object.text
        )


ctx = lal.AnalysisContext()
u = ctx.get_from_file("test.adb")

for call in u.root.findall(lal.DottedName):
    called = call.p_referenced_decl().p_body_part
    a = called.find(lal.AssignStmt)
    print("In instantiation of {}:".format(a.p_generic_instantiations[0]))
    print("  - X.p_referenced_decl() => {}".format(
        format_object_decl(a.f_dest.p_referenced_decl())
    ))
    print("  - Y.p_referenced_decl() => {}".format(
        format_object_decl(a.f_expr.p_referenced_decl())
    ))
    print("")

print("Done")
