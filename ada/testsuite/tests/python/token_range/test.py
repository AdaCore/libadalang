import libadalang


ctx = libadalang.AnalysisContext()
u = ctx.get_from_file('foo.adb')

print('AST root start and end tokens:')
print(u.root.token_start)
print(u.root.token_end)
print 'Done.'
