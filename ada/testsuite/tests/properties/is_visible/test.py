import sys

import libadalang as lal


ctx = lal.AnalysisContext(
    unit_provider=lal.UnitProvider.for_project("test.gpr")
)
test_decls = []
origin_decls = []
units = [ctx.get_from_file(f) for f in sys.argv[1:]]

for u in units:
    u.populate_lexical_env()
    test_decls.extend(u.root.findall(
        lambda n:
        n.is_a(lal.BasicDecl) and
        n.p_defining_name.text.startswith("Test")
    ))
    origin_decls.extend(u.root.findall(
        lambda n:
        n.is_a(lal.BasicDecl) and
        n.p_defining_name.text.startswith("Origin")
    ))


for test_decl in test_decls:
    for origin_decl in origin_decls:
        test_name = test_decl.p_defining_name.text
        origin_name = origin_decl.p_defining_name.text

        print("{: <25} is visible to {: >28} ? {}".format(
            test_name, origin_name, test_decl.p_is_visible(origin_decl)
        ))

    print("")

print("Done")
