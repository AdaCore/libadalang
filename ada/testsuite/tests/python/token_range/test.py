from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

import libadalang


ctx = libadalang.AnalysisContext()
for with_trivia in (False, True):
    print('With_Trivia => {}'.format(with_trivia))
    u = ctx.get_from_file('foo.adb',
                          with_trivia=with_trivia,
                          reparse=True)

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
