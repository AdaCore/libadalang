from __future__ import absolute_import, division, print_function

import sys

import libadalang as lal


for filename in sys.argv[1:]:
    print('== {} =='.format(filename))
    u = lal.AnalysisContext().get_from_file(filename)
    assert not u.diagnostics

    for assocs in u.root.findall(lal.AssocList):
        binds = assocs.p_zip_with_params()
        print("Binds for {} : {}".format(assocs, [
            "{}: {}".format(b.param.p_defining_names[0].text, b.actual.text)
            for b in binds
        ]))

    print('')

print('Done')
