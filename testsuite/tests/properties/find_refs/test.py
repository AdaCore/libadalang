import sys

import libadalang as lal


for filename in sys.argv[1:]:
    print('== {} =='.format(filename))
    u = lal.AnalysisContext().get_from_file(filename)
    assert not u.diagnostics

    for b in u.root.findall(lal.SubpBody):
        for p in b.f_subp_spec.p_params:
            for n in p.p_defining_names:
                print(n.p_find_refs(b))

    print('')

print('Done')
