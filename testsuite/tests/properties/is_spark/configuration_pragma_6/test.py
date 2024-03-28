"""
This test checks that calling get_aspect/has_aspect/get_pragma/is_spark on a
subprogram body node works as expected.
"""
import glob
import libadalang as lal


project = lal.GPRProject('test.gpr')
context = project.create_context()

for filename in sorted(glob.glob('*.ad[bs]')):
    unit = context.get_from_file(filename)
    subprograms = unit.root.findall(
        lambda n: n.is_a(lal.BaseSubpBody, lal.BasicSubpDecl)
    )
    if subprograms:
        print('=== Analysing unit: ', unit, '===')
        for subprogram in subprograms:
            print("Subprogram {} is {}".format(
                subprogram,
                "analyzed" if subprogram.p_has_spark_mode_on else "skipped"
            ))
        print('')
