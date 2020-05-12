#! /usr/bin/env python

"""
This script will detect unsynchronized boxes, where the name of the subprogram
and the subprogram do not match, such as::

    ---------------
    -- Wrong_Box --
    ---------------

    function Right_Box return Boolean;

If passed the --fix flag, it will fix the boxes in the same pass.
"""

import argparse
import libadalang as lal


parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('files', help='The files to analyze',
                    type=str, nargs='+', metavar='F')
parser.add_argument('--fix', action='store_true', help='Fix the code')

content = []


def check_unsync_box(subp_body):
    """
    Check if subp_body has an unsynchronized box. If it does, returns the first
    token of the box. If it doesn't return None.

    :type subp_body: lal.SubpBody
    :rtype: lal.Token|None
    """
    ss = subp_body.f_subp_spec

    # Library unit subprogram with composed name
    if type(ss.f_subp_name) is not lal.Identifier:
        return

    subp_name = ss.f_subp_name.f_tok.text
    t = subp_body.token_start.previous

    # Toplevel subprogram, no previous token
    if not t:
        return

    t2 = t.previous
    t3 = t2.previous
    ts = [t, t2, t3]
    if all(t.kind == 'Comment' for t in ts) and all(
        c == '-' for c in t.text + t3.text
    ):
        if t2.text != "-- {} --".format(subp_name):
            return t3


def main(args):
    c = lal.AnalysisContext('utf-8')

    for f in args.files:
        # Get the file's content, for error reporting and for fixing
        with open(f) as ff:
            content = ff.read().splitlines()

        # Parse, and report errors on fail
        unit = c.get_from_file(f, with_trivia=True)
        if unit.root is None:
            print('Could not parse {}:'.format(f))
            for diag in unit.diagnostics:
                print('   {}'.format(diag))
            continue

        # If successful, check boxes for every subprogram body
        for sb in unit.root.findall(lal.SubpBody):

            first_comment = check_unsync_box(sb)

            # Box is fine, or no box, continue
            if not first_comment:
                continue

            # Box is malformed, report the error
            subp_name = sb.f_subp_spec.f_subp_name.f_tok.text
            first_line = first_comment.sloc_range.start.line
            print("{}:{}:{}: Malformed box for subprogram '{}'".format(
                f, sb.sloc_range.start.line, sb.sloc_range.start.column,
                subp_name
            ))
            for i in range(
                first_line,
                sb.f_subp_spec.sloc_range.end.line + 1
            ):
                print(content[i - 1])
            print()

            # If user asked to fix malformed boxes, replace the box in the
            # source list.
            if args.fix:
                indent = ' ' * (sb.sloc_range.start.column - 1)
                content[first_line - 1] = indent + ('-' * (len(subp_name) + 6))
                content[first_line] = "{}-- {} --".format(indent, subp_name)
                content[first_line + 1] = indent + ('-' * (len(subp_name) + 6))

                # And then dump to file again
                with open(f, 'w') as ff:
                    ff.write('\n'.join(content) + '\n')


if __name__ == '__main__':
    main(parser.parse_args())
