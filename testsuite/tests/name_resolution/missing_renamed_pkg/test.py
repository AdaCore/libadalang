import libadalang as lal


ctx = lal.AnalysisContext(event_handler=lal.App.CommandLineEventHandler(
    keep_going_on_missing_file=True
))

test_unit = ctx.get_from_file("test.adb")
pkg_unit = ctx.get_from_file("pkg.ads")


print(test_unit.root.p_decl.p_defining_name.p_find_all_references([pkg_unit]))
