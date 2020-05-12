import libadalang as lal
import sys


def print_attributes(n):
    print("{} get_representation_clause('size') -> {}".format(
        n, n.p_get_representation_clause("size")
    ))
    print("{} get_representation_clause('alignment') -> {}".format(
        n, n.p_get_representation_clause("alignment")
    ))


for f in sys.argv[1:]:
    u = lal.AnalysisContext().get_from_file(f)
    assert not u.diagnostics

    tdecls = u.root.findall(lal.BaseTypeDecl)

    print_attributes(tdecls[0])  # should print none as tdecls[0] is U16
    print_attributes(tdecls[1])  # should print both repr clauses of Arr

    print('')
    print('Done')
