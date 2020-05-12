import libadalang as lal
import sys


for f in sys.argv[1:]:
    u = lal.AnalysisContext().get_from_file(f)
    assert not u.diagnostics

    n = u.root.findall(lal.BaseTypeDecl)[0]
    print("{} get_record_representation_clause -> {}".format(
        n, n.p_get_record_representation_clause()
    ))
    print('')
    print('Done')
