import libadalang

ctx = libadalang.AnalysisContext()
try:
    unit = ctx.create_from_file('foo.adb')
except IOError:
    pass
else:
    assert False, (
        'Creating an analysis unit for foo.adb worked whereas there is'
        ' no such file'
    )

print 'Done.'
