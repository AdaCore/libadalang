import sys

import libadalang as lal


for filename in sys.argv[1:]:
    print('== {} =='.format(filename))
    u = lal.AnalysisContext().get_from_file(filename)
    assert not u.diagnostics

    for n in u.root.findall(lal.DottedName):
        is_dot_call_precise = n.p_is_dot_call(imprecise_fallback=False)
        is_dot_call_imprecise = n.p_is_dot_call(imprecise_fallback=True)

        print("{} is a dot call? {}. With imprecise fallback: {}".format(
            n, is_dot_call_precise, is_dot_call_imprecise
        ))

    print('')

print('Done')
