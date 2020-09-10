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
from os import path as P

import libadalang as lal

from langkit.diagnostics import Location, print_error


content = []


def check_unsync_box(subp_body):
    """
    Check the box of ``subp_body``:

    - If it has none, return the first token of the subprogram.
    - If it has one but it is malformed, return the first token of the box.
    - If it has one and it's good, or if it is a toplevel subprogram and
      doesn't need a box, return None.

    :type subp_body: lal.SubpBody
    :rtype: lal.Token|None
    """

    def prev(tok):
        while (tok := tok.previous) is not None and tok.kind == 'Whitespace':
            continue
        return tok

    ss = subp_body.f_subp_spec

    # Library unit subprogram with composed name
    if type(ss.f_subp_name.f_name) is not lal.Identifier:
        return

    subp_name = ss.f_subp_name.f_name.text
    t = prev(subp_body.token_start)

    # Toplevel subprogram, no previous token
    if not t:
        return

    t2 = prev(t)
    t3 = prev(t2)
    ts = [t, t2, t3]

    if all(t.kind == 'Comment' for t in ts) and all(
        c == '-' for c in t.text + t3.text
    ):
        if t2.text != f"-- {subp_name} --":
            return t3
        else:
            return None
    else:
        return subp_body.token_start


class CheckSubpBoxes(lal.App):
    def add_arguments(self):
        self.parser.add_argument(
            '--fix', action='store_true',
            default=False,
            help='Fix found errors'
        )
        super(CheckSubpBoxes, self).add_arguments()

    def process_unit(self, unit):
        print(f"Checking {P.basename(unit.filename)}")

        content_changed = False
        # Get the file's content, for error reporting and for fixing
        content = unit.text.splitlines()
        add_content = {}

        # Parse, and report errors on fail
        if unit.root is None:
            print(f'Could not parse {f}:')
            for diag in unit.diagnostics:
                print_error(
                    diag.message,
                    Location.from_sloc_range(unit, diag.sloc_range)
                )
                print(f'   {diag}')

        # If successful, check boxes for every subprogram body
        for sb in unit.root.findall(lal.SubpBody):

            first_comment = check_unsync_box(sb)

            # Box is fine, or no box, continue
            if not first_comment:
                continue

            subp_name_node = sb.f_subp_spec.f_subp_name
            subp_name = subp_name_node.text

            # Box is malformed, report the error
            if first_comment == sb.token_start:
                print_error(
                    f"No box for subprogram `{subp_name}`",
                    Location.from_sloc_range(unit, subp_name_node.sloc_range)
                )
                # If user asked to fix boxes, replace the box in the source
                # list.
                if self.args.fix:
                    content_changed = True
                    first_line = first_comment.sloc_range.start.line
                    indent = ' ' * (sb.sloc_range.start.column - 1)
                    add_content[first_line - 1] = [
                        indent + ('-' * (len(subp_name) + 6)),
                        f"{indent}-- {subp_name} --",
                        indent + ('-' * (len(subp_name) + 6)),
                        ''
                    ]
            else:
                first_line = first_comment.sloc_range.start.line
                print_error(
                    f"Malformed box for subprogram `{subp_name}`",
                    Location.from_sloc_range(unit,
                                             first_comment.next.sloc_range)
                )

                # If user asked to fix boxes, replace the box in the source
                # list.
                if self.args.fix:
                    content_changed = True
                    box_dashes = indent + ("-" * (len(subp_name) + 6))
                    indent = ' ' * (sb.sloc_range.start.column - 1)
                    content[first_line - 1] = box_dashes
                    content[first_line] = f"{indent}-- {subp_name} --"
                    content[first_line + 1] = box_dashes

        if content_changed:
            # Then dump to file again
            with open(unit.filename, 'w') as f:
                for i, l in enumerate(content):
                    if i in add_content:
                        for line in add_content[i]:
                            f.write(f"{line}\n")
                    f.write(f"{l}\n")


if __name__ == '__main__':
    CheckSubpBoxes.run()
