from __future__ import absolute_import, division, print_function

import libadalang as lal


ctx = lal.AnalysisContext()
u = ctx.get_from_file("test.adb")

for call in u.root.findall(lal.DottedName):
    called = call.p_referenced_decl().p_body_part
    a = called.find(lal.AssignStmt)
    print("In instantiation of {}:".format(a.p_generic_instantiations[0]))
    print("  - X.p_eval_as_int() => {}".format(
        a.f_dest.p_eval_as_int
    ))
    print("  - Y.p_eval_as_int() => {}".format(
        a.f_expr.p_eval_as_int
    ))
    print("")

print("Done")
