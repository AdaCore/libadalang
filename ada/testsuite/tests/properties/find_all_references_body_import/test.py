from __future__ import absolute_import, division, print_function

import os
import sys

import libadalang as lal


ctx = lal.AnalysisContext(unit_provider=lal.UnitProvider.auto(sys.argv[1:]))
all_units = [
    ctx.get_from_file(f) for f in sys.argv[1:]
]


for unit in all_units:
    if len(unit.diagnostics) > 0:
        print(unit.diagnostics)


hello_adb = ctx.get_from_file("hello.adb")
hello = hello_adb.root.find(lal.DefiningName)


print('All references to Hello from hello.adb:')
for ref in hello.p_find_all_references(all_units):
    while ref.parent is not None and not ref.p_xref_entry_point:
        ref = ref.parent

    print('    {} ({}, {})'.format(
        ref.text.splitlines()[0],
        os.path.basename(ref.unit.filename),
        ref.sloc_range)
    )

print('Done')
