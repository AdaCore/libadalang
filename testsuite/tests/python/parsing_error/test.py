import libadalang


ctx = libadalang.AnalysisContext()
unit = ctx.get_from_file('foo.adb')

assert unit.root is None, (
    'Got a non-null tree for a source with fatal syntax errors'
)

for diag in unit.diagnostics:
    print 'Diagnostic: {}:{}-{}:{}: {}'.format(
        diag.sloc_range.start.line,
        diag.sloc_range.start.column,
        diag.sloc_range.end.line,
        diag.sloc_range.end.column,
        diag.message
    )

print 'Done.'
