from __future__ import absolute_import, division, print_function

import libadalang as lal


ctx = lal.AnalysisContext()
u = ctx.get_from_file("test.adb")

assert not u.diagnostics

procs = u.root.findall(
    lambda n: n.is_a(lal.BasicDecl) and n.p_defining_name.text == "Foo"
)
for proc in procs:
    print("The root declarations of {} are {}".format(
        proc, proc.p_root_subp_declarations()
    ))

print('Done')
