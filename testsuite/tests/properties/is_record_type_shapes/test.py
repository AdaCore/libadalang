import sys

import libadalang as lal


for filename in sys.argv[1:]:
    print('== {} =='.format(filename))
    u = lal.AnalysisContext().get_from_file(filename)
    assert not u.diagnostics

    for n in u.root.findall(lal.TypeDecl):
        if n.p_is_record_type():
            print(n, n.p_shapes())
        else:
            print(n, "not a record")

    print('')

print('Done')
