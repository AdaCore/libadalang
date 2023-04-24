import sys

import libadalang as lal


ctx = lal.AnalysisContext()
units = [ctx.get_from_file(f) for f in sys.argv[1:]]
for u in units:
    cu = u.root
    has_restriction = cu.p_has_restriction("no_elaboration_code")
    print("{} {} restriction No_Elaboration_Code".format(
        cu, "has" if has_restriction else "does not have"
    ))
