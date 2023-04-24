"""
This test checks that calling base_subp_declarations on a subprogram node
that was retrieved by a previous call to base_subp_declarations works as
expected.
"""
import libadalang as lal


def subp_image(subp):
    return "{}:{}".format(subp.p_defining_name.text, subp.sloc_range)


ctx = lal.AnalysisContext()
u = ctx.get_from_file("test.adb")

subps = u.root.findall(lal.NullSubpDecl)
for subp in subps:
    print("Base declarations of {}:".format(subp_image(subp)))
    bases = subp.p_base_subp_declarations()
    for base in bases:
        print("  - {}".format(subp_image(base)))
        base_bases = base.p_base_subp_declarations()
        for base_base in base_bases:
            print("      - {}".format(subp_image(base_base)))
