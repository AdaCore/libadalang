import libadalang


ctx = libadalang.AnalysisContext()
u = ctx.get_from_file('foo.adb', with_trivia=True)

prev_token = None
for token in u.iter_tokens():
    assert prev_token == token.previous, "Inconsistent previous token"
    print('{}{}'.format(
        token.kind,
        ' {!r}'.format(token.text) if token.text else ''
    ))
    prev_token = token
print 'Done.'
