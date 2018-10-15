from __future__ import absolute_import, division, print_function

import sys

import libadalang as lal


ctx = lal.AnalysisContext(unit_provider=lal.UnitProvider.auto(sys.argv[1:]))
all_units = [
    ctx.get_from_file(f) for f in sys.argv[1:]
]


for unit in all_units:
    if len(unit.diagnostics) > 0:
        print(unit.diagnostics)


def find_name(unit_name, name_text, line_no=None):
    unit = ctx.get_from_file(unit_name)
    return unit.root.find(
        lambda x: (
            x.is_a(lal.DefiningName) and x.text == name_text and
            line_no is None or x.sloc_range.start.line == line_no
        )
    )


def do_test(name):
    for ref in name.p_find_all_references(all_units):
        print(ref.text, ":", ref.unit, ref.sloc_range)


do_test(find_name("a.ads", "A"))
do_test(find_name("a.ads", "X"))
do_test(find_name("a.adb", "Y"))
do_test(find_name("a.adb", "Get_X"))
do_test(find_name("a.ads", "U"))
do_test(find_name("a.ads", "Rec_Type"))
do_test(find_name("b.adb", "X"))
do_test(find_name("c.ads", "Foo"))

print('Done')
