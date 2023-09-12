"""
This test checks that calling get_aspect/has_aspect/get_pragma/is_spark on a
subprogram body node works as expected.
"""
import glob
import libadalang as lal


context = lal.AnalysisContext()

for filename in sorted(glob.glob('*.ad[bs]')):
    unit = context.get_from_file(filename)
    subprograms = unit.root.findall(lal.BaseSubpBody)
    if subprograms:
        print('=== Analysing unit: ', unit, '===')
        for subprogram in subprograms:
            print("Subprogram {} is {}".format(
                subprogram,
                "analyzed" if subprogram.p_is_spark() else "skipped"
            ))
        print('')
