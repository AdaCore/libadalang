import libadalang

ctx = libadalang.AnalysisContext()
unit = ctx.create_from_file('foo.adb')
assert unit.root is None, (
    'Got a non-null tree for a source with fatal syntax errors'
)
print 'Done.'
