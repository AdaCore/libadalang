from __future__ import absolute_import, division, print_function

import sys
import libadalang


for with_trivia in (False, True):
    print('With_Trivia => {}'.format(with_trivia))
    sys.stdout.flush()
    ctx = libadalang.AnalysisContext(with_trivia=with_trivia)
    print('---')
    with open('foo.adb', 'rb') as f:
        print('>>> {}'.format(repr(f.read())))
    sys.stdout.flush()
    u = ctx.get_from_file('foo.adb')
    print('==')
    with open('foo.adb', 'rb') as f:
        print('>>> {}'.format(repr(f.read())))
    sys.stdout.flush()

    print('Unit has {} tokens and {} trivias'.format(
        u.token_count, u.trivia_count))
    print('Unit start and end tokens:')
    print('  {}'.format(u.first_token))
    print('  {}'.format(u.last_token))
    print('AST root start and end tokens:')
    print('  {}'.format(u.root.token_start))
    print('  {}'.format(u.root.token_end))
    print('')

print('Done.')
