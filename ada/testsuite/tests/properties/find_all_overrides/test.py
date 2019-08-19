from __future__ import absolute_import, division, print_function

import libadalang as lal


ctx = lal.AnalysisContext()
u = ctx.get_from_file("test.adb")

assert not u.diagnostics

def subp_fqn(subp):
    return "{} (line {})".format(
        subp.p_fully_qualified_name,
        subp.sloc_range.start.line
    )

subps = u.root.findall(
    lambda n: n.is_a(lal.BasicDecl) and n.p_is_subprogram
)

for subp in subps:
    print("Subprograms overriding {}:".format(subp_fqn(subp)))
    for s in subp.p_find_all_overrides([u]):
        print("  {}".format(subp_fqn(s)))

print('Done')
