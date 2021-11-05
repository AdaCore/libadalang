import libadalang as lal


def format_object_decl(decl):
    return "<AnonymousExprDecl {} = {}>".format(
        decl.p_type_expression.text,
        decl.f_expr.text
    )


ctx = lal.AnalysisContext()
u = ctx.get_from_file("test.adb")

for call in u.root.findall(lal.CallStmt):
    called_decl = call.f_call.p_referenced_decl().p_designated_generic_decl
    called_body = called_decl.p_body_part()
    a = called_body.find(lal.AssignStmt)
    print("In instantiation of {}:".format(a.p_generic_instantiations[0]))

    dest_decl = a.f_dest.p_referenced_decl()
    print("  - X.p_referenced_decl() => {}".format(
        format_object_decl(dest_decl)
    ))
    print("    - p_get_formal() => {}".format(
        dest_decl.p_get_formal(())
    ))

    expr_decl = a.f_expr.p_referenced_decl()
    print("  - Y.p_referenced_decl() => {}".format(
        format_object_decl(expr_decl)
    ))
    print("    - p_get_formal() => {}".format(
        expr_decl.p_get_formal()
    ))
    print("")

print("Done")
