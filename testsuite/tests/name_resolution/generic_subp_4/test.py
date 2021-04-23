import libadalang as lal


ctx = lal.AnalysisContext()
u = ctx.get_from_file("test.adb")
ce = u.root.find(lal.CallStmt).f_call.p_referenced_decl()
print(ce)
