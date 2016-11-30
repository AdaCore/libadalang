import libadalang

for project_file in ('invalid.gpr', 'does_not_exist.gpr'):
    print('Trying to load invalid project: {}'.format(project_file))
    try:
        ufp = libadalang.UnitFileProvider.for_project('invalid.gpr')
    except libadalang.InvalidProjectError as exc:
        print('   ... got an exception: {}'.format(exc))
    else:
        print('   ... got no exception. Unacceptable!')

ctx = libadalang.AnalysisContext(
    unit_file_provider=libadalang.UnitFileProvider.for_project('p.gpr')
)

for filename in ('\n', ' '):
    print('Trying to get unit: {}'.format(repr(filename)))
    try:
        unit = ctx.get_from_provider(filename, 'body')
    except libadalang.InvalidUnitNameError as exc:
        print('   ... got an exception: {}'.format(exc))
    else:
        print('   ... got no exception. Unacceptable!')

print "Done."
