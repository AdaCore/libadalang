from __future__ import absolute_import, division, print_function

import os

import libadalang as lal


ctx = lal.AnalysisContext()
unit = ctx.get_from_file('test.adb')


if len(unit.diagnostics) > 0:
    print(unit.diagnostics)


main = unit.root.find(lal.SubpBody)


for subp in main.findall(lal.BasicDecl):
    if not subp.p_is_subprogram:
        continue
    node = subp.p_defining_name
    print('{} at line {} is called by:'.format(
        node.text,
        node.sloc_range.start.line
    ))
    for ref in node.p_find_all_calls([unit]):
        while ref.parent is not None and not ref.p_xref_entry_point:
            ref = ref.parent

        print('    {} ({}, {})'.format(
            ref.text.splitlines()[0],
            os.path.basename(ref.unit.filename),
            ref.sloc_range)
        )

print('Done')
