from __future__ import absolute_import, division, print_function

import os
import sys

import libadalang as lal


ctx = lal.AnalysisContext()
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

for unit, name in [('test.adb', 'Foo'), ('base.ads', 'Bar'),
                   ('derived.ads', 'Bar'), ('derived_3.ads', 'Bar')]:
    print('{} from {} is called by:'.format(name, unit))
    for ref in find_name(unit, name).p_is_called_by(all_units):
        while ref.parent is not None and not ref.p_xref_entry_point:
            ref = ref.parent

        print('    {} ({}, {})'.format(
            ref.text.splitlines()[0],
            os.path.basename(ref.unit.filename),
            ref.sloc_range)
        )

print('Done')
