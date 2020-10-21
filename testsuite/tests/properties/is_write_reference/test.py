import sys

import libadalang as lal


for filename in sys.argv[1:]:
    print('== {} =='.format(filename))
    u = lal.AnalysisContext().get_from_file(filename)
    assert not u.diagnostics

    for name in u.root.findall(lal.DefiningName):
        print("Analyzing references of {} ({})".format(name.text, name))
        for ref in name.p_find_all_references([u]):
            print("  Reference {} is {}".format(
                ref,
                "a write reference." if ref.ref.p_is_write_reference()
                else "not a write reference."
            ))
        print('')

    print('')

print('Done')
