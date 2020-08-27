import sys

import libadalang as lal


for filename in sys.argv[1:]:
    print('== {} =='.format(filename))
    u = lal.AnalysisContext().get_from_file(filename)
    assert not u.diagnostics

    for n in u.root.findall(lal.BasicDecl):
        print('{} canonical part -> {}'.format(n, n.p_canonical_part()))

    print('')

print('Done')
