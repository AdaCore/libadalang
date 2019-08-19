from __future__ import absolute_import, division, print_function

import libadalang as lal


ctx = lal.AnalysisContext()
u = ctx.get_from_file("test.adb")

assert not u.diagnostics


def find_name(unit_name, name_text, line_no=None):
    unit = ctx.get_from_file(unit_name)
    return unit.root.find(
        lambda x: (
            x.is_a(lal.DefiningName) and x.text == name_text and
            line_no is None or x.sloc_range.start.line == line_no
        )
    )


call = u.root.find(lal.CallExpr)
integer_io_get = call.p_referenced_decl().f_subp_spec.f_subp_name
refs = integer_io_get.p_find_all_references([u])

print("References to Ada.Text_IO.Integer_IO.Get:")
for ref in refs:
    print(ref)

print('Done')
