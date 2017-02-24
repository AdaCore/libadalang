from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

import libadalang as lal


def process(filename, with_trivia):
    unit = ctx.get_from_file(filename, with_trivia=with_trivia)
    token = unit.first_token
    prev_token = None

    print('Tokens for {} ({} trivia):'.format(
        filename,
        'with' if with_trivia else 'no'
    ))
    while token:
        pt = token.previous
        assert pt == prev_token

        print('  [{typ} {index}] {kind} {image}'.format(
            typ='trivia' if token.is_trivia else 'token ',
            index=token.index,
            kind=token.kind,
            image=repr(token.text),
        ))
        prev_token = token
        token = token.next

    print('')
    ctx.remove(filename)


ctx = lal.AnalysisContext()
for filename in ('no_trivia.adb', 'empty.adb'):
    process(filename, False)

for filename in (
    "one_leading_comment.adb",
    "two_leading_comments.adb",
    "one_middle_comment.adb",
    "two_middle_comments.adb",
    "one_trailing_comment.adb",
    "two_trailing_comments.adb",
    "only_one_comment.adb",
    "only_two_comments.adb"
):
    process(filename, True)

print('Done.')
