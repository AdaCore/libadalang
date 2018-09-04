from __future__ import absolute_import, division, print_function

import sys

import libadalang as lal


for filename in sys.argv[1:]:
    print('== {} =='.format(filename))
    u = lal.AnalysisContext().get_from_file(filename)
    assert not u.diagnostics

    for n in u.root.findall(lal.PragmaNode):
        print('{} -> {} ->> {}'.format(
            n,
            n.p_associated_decls,
            n.p_associated_decls[0].p_get_pragma(n.f_id.text)
        ))

    print('')

print('Done')
