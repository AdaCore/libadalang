import libadalang

ctx = libadalang.AnalysisContext('iso-8859-1')
try:
    unit = ctx.get_from_file('foo.adb')
except IOError:
    pass
else:
    assert False, (
        'Creating an analysis unit for foo.adb worked whereas there is'
        ' no such file'
    )

print 'Done.'
