import sys

import libadalang as lal


ctx = lal.AnalysisContext()

print('Is preelaborable?')
for filename in sys.argv[1:]:
    u = ctx.get_from_file(filename)
    print('{: <15} {}'.format(filename + ':', u.root.p_is_preelaborable()))

print('Done')
