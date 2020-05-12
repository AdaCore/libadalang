import libadalang
from libadalang import _py2to3


ctx = libadalang.AnalysisContext(with_trivia=True)
u = ctx.get_from_file('foo.adb')

prev_token = None
for token in u.iter_tokens():
    assert prev_token == token.previous, 'Inconsistent previous token'
    print('{}{}'.format(
        token.kind,
        ' {}'.format(_py2to3.text_repr(token.text)) if token.text else ''
    ))
    prev_token = token
print('Done.')
