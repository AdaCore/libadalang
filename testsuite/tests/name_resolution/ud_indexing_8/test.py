import libadalang as lal
import sys


for filename in sys.argv[1:]:
    print("== {} ==\n".format(filename))
    u = lal.AnalysisContext().get_from_file(filename)

    for ce in u.root.p_decl.findall(lal.CallExpr):
        od = ce.f_name.p_referenced_decl()
        # Only print for CallExpr that resolves to an ObjectDecl with
        # user-indexing functions.
        if od.is_a(lal.ObjectDecl):
            td = od.f_type_expr.p_designated_type_decl
            if td.p_has_aspect("Constant_Indexing") or td.p_has_aspect(
                "Variable_Indexing"
            ):
                print(
                    "-> Resolving {} to {}\n".format(
                        ce, ce.p_called_subp_spec.f_subp_name
                    )
                )
