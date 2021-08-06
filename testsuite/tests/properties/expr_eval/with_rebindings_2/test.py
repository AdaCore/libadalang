import libadalang as lal


ctx = lal.AnalysisContext()
u = ctx.get_from_file("test.adb")

V_decl = u.root.findall(lal.ObjectDecl)[1]
R_Arr_ref = V_decl.f_type_expr
R_Arr_decl = R_Arr_ref.p_designated_type_decl
R_ref = (R_Arr_decl.f_type_def.f_subtype_indication
         .f_constraint.f_constraints[0].f_constraint_expr)
R_decl = R_ref.p_referenced_decl()
R_range = R_decl.p_discrete_range

print("In instantiation {}:".format(R_Arr_decl.p_generic_instantiations[0]))
print("  R_Arr's index range is {} .. {}".format(
    R_range.low_bound.p_eval_as_int,
    R_range.high_bound.p_eval_as_int
))

print("Done")
