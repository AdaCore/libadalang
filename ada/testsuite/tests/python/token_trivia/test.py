import libadalang


ctx = libadalang.AnalysisContext()
u = ctx.get_from_file('foo.adb', with_trivia=True)

for token in u.iter_tokens():
    print('{}{}'.format(
        token.kind,
        ' {!r}'.format(token.text) if token.text else ''
    ))
print 'Done.'
