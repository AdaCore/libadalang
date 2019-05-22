from __future__ import absolute_import, division, print_function

import sys

import libadalang as lal


for filename in sys.argv[1:]:
    print('== {} =='.format(filename))
    u = lal.AnalysisContext().get_from_file(filename)
    assert not u.diagnostics

    print("Is call    Entity")
    print("-" * 50)
    for n in u.root.findall(lal.Name):
        if not n.p_is_defining:
            print('{}        {}'.format(
                'YES' if n.p_is_call else 'NO ',
                n
            ))

    print('')

print('Done')
