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
    print("  - X.p_eval_as_int() => {}".format(
        format_object_decl(a.f_dest.p_referenced_decl())
    ))
    print("  - Y.p_eval_as_int() => {}".format(
        format_object_decl(a.f_expr.p_referenced_decl())
    ))
    print("")

print("Done")
