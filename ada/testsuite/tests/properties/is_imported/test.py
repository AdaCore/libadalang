import sys

import libadalang as lal


for filename in sys.argv[1:]:
    print('== {} =='.format(filename))
    u = lal.AnalysisContext().get_from_file(filename)
    assert not u.diagnostics

    for n in u.root.findall(lal.BasicDecl):
        print('{} -> {}'.format(n, n.p_is_imported))

    print('')

print('Done')
