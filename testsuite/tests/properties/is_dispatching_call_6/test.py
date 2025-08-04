import libadalang as lal


ctx = lal.AnalysisContext()
u = ctx.get_from_file('test.adb')
assert not u.diagnostics

root_subp_body = u.root.findall(lal.SubpBody)[0]

for stmt in root_subp_body.f_stmts.f_stmts:
    print("Processing calls in: {}\n".format(stmt))
    for name in stmt.findall(lal.Name):
        if name.p_is_call:
            print("Call to {} is dispatching call: {}".format(
                name, name.p_is_dispatching_call()
            ))
    print('')

print('')

print('Done')
