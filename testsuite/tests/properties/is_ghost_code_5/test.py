import libadalang as lal


ctx = lal.AnalysisContext()


def test_file(filename):
    u = ctx.get_from_file(filename)
    inst = u.root.find(lal.GenericPackageInstantiation)
    decl = inst.p_designated_generic_decl
    print(f"{decl} is ghost ? {decl.p_is_ghost_code}")


test_file("test_1.adb")
test_file("test_2.adb")
