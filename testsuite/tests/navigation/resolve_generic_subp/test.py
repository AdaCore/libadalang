import libadalang as lal

ctx = lal.AnalysisContext()
u = ctx.get_from_file("test.adb")
call = u.root.findall(lal.CallStmt)[-1]
print("In call " + call.text)
subp = call.f_call.p_referenced_decl()
body = subp.p_body_part()
call = body.find(lal.CallStmt).f_call
print("In call " + call.text)
subp = call.p_referenced_decl()
body = subp.p_body_part()
call = body.find(lal.CallStmt).f_call
print("In call " + call.text)
print("The actual for generic subprogram Foo resolves to:")
print(call.p_referenced_decl())

