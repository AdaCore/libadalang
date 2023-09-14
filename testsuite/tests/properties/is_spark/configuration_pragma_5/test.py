"""
This test checks that calling get_aspect/has_aspect/get_pragma/is_spark on a
subprogram body node works as expected.
"""
import glob
import libadalang as lal


context = lal.AnalysisContext()
context.set_config_pragmas_mapping(None, {})

for filename in sorted(glob.glob('*.ad[bs]')):
    unit = context.get_from_file(filename)
    subprograms = unit.root.findall(lal.BaseSubpBody)
    if subprograms:
        print('=== Analysing unit: ', unit, '"""')
        for subprogram in subprograms:
            print("Subprogram {} is {}".format(
                subprogram,
                "analyzed" if subprogram.p_is_subject_to_proof else "skipped"
            ))
        print('')
