from glob import glob
from os import path

import libadalang as lal


ctx = lal.AnalysisContext(
    unit_provider=lal.UnitProvider.for_project("prj.gpr")
)


def identify(cu):
    return "{} (from {})".format(
        ".".join(cu.p_syntactic_fully_qualified_name),
        path.basename(cu.unit.filename)
    )


def process_compilation_unit(cu):
    print("Unit dependencies of {}:".format(identify(cu)))
    for dep in cu.p_unit_dependencies:
        print("  - {}".format(identify(dep)))


for source_name in sorted(glob("*.ad?")):
    u = ctx.get_from_file(source_name)
    assert not u.diagnostics
    if u.root.is_a(lal.CompilationUnit):
        process_compilation_unit(u.root)
    else:
        for cu in u.root.findall(lal.CompilationUnit):
            process_compilation_unit(cu)
    print("")

print('Done')
