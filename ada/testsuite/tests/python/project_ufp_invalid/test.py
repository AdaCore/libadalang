import libadalang

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
