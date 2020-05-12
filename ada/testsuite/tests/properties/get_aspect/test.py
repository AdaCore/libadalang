import sys

import libadalang as lal


for filename in sys.argv[1:]:
    print('== {} =='.format(filename))
    u = lal.AnalysisContext().get_from_file(filename)
    assert not u.diagnostics

    for n in u.root.findall(lal.SubpDecl):
        print("{}:".format(n))
        print("  import aspect: {}".format(n.p_get_aspect("import")))
        print("  post'class aspect: {}".format(n.p_get_aspect("post'class")))

    print('')

print('Done')
