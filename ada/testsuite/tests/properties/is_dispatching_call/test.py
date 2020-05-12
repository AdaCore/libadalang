import libadalang as lal


ctx = lal.AnalysisContext()
u = ctx.get_from_file('test.adb')
assert not u.diagnostics

root_pkg = u.root.findall(lal.PackageDecl)[0]

for subp in root_pkg.findall((lal.NullSubpDecl, lal.ExprFunction)):
    name = subp.f_subp_spec.f_subp_name
    print("Analyzing references of {} ({})".format(name.text, name))
    for ref in name.p_find_all_references([u]):
        if ref.ref.p_is_call:
            print("  Reference {} is {}".format(
                ref,
                "a dispatching call." if ref.ref.p_is_dispatching_call()
                else "not a dispatching call."
            ))

print('')

print('Done')
