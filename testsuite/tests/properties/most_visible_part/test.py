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
        n.is_a(lal.TypeDecl) and
        n.p_defining_name.text.startswith("Test") and
        n.p_is_private
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
        completion = test_decl.p_most_visible_part(origin_decl)
        is_private_view = completion.p_is_private

        if is_private_view:
            print("{: <28} sees the public  part of {}".format(
                origin_name, test_name
            ))
        else:
            print("{: <28} sees the private part of {}".format(
                origin_name, test_name
            ))

    print("")

print("Done")
