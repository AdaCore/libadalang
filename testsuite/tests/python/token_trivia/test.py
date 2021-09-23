import libadalang


ctx = libadalang.AnalysisContext(with_trivia=True)
u = ctx.get_from_file('foo.adb')

prev_token = None
for token in u.iter_tokens():
    assert prev_token == token.previous, 'Inconsistent previous token'
    print('{}{}'.format(
        token.kind,
        ' {}'.format(repr(token.text)) if token.text else ''
    ))
    prev_token = token
print('Done.')
