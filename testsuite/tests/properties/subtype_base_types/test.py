import sys

import libadalang as lal


for filename in sys.argv[1:]:
    print('== {} =='.format(filename))
    u = lal.AnalysisContext().get_from_file(filename)
    assert not u.diagnostics

    std = u.root.find(lal.SubtypeDecl)
    print("{}:".format(std))
    print("  base types: {}".format(std.p_base_types()))

    print('')

print('Done')
