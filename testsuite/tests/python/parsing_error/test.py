import libadalang


ctx = libadalang.AnalysisContext()
unit = ctx.get_from_file('foo.adb')

for diag in unit.diagnostics:
    print 'Diagnostic: {}:{}-{}:{}: {}'.format(
        diag.sloc_range.start.line,
        diag.sloc_range.start.column,
        diag.sloc_range.end.line,
        diag.sloc_range.end.column,
        diag.message
    )

print 'Done.'
