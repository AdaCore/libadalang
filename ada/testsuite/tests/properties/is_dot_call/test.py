import sys

import libadalang as lal


for filename in sys.argv[1:]:
    print('== {} =='.format(filename))
    u = lal.AnalysisContext().get_from_file(filename)
    assert not u.diagnostics

    for n in u.root.findall(lal.DottedName):
        if n.p_is_dot_call():
            print('{} is a dot call'.format(n))
        else:
            print('{} is not a dot call'.format(n))

    print('')

print('Done')
