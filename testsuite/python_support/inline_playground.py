#!/usr/bin/env python

import os
import pprint
import sys

import libadalang as lal


desc = """
Go over Ada files given as argument, processing any lines starting with --% and
containing a python expression starting with $. For those lines, eval the
expression in a context containing the following variables:

- `node` is the ast node immediately preceding the comment.
- `lal` is the libadalang module.
"""


YELLOW = '\033[33m'


def col(msg, color):
    return msg if not sys.stdout.isatty() else color + msg + '\033[0m'


class InlinePlayground(lal.App):

    def main(self):
        # Sort unit by filename to have a deterministic processing order
        for _, unit in sorted(self.units.items()):
            self.process_unit(unit)

    def description(self):
        return desc

    def add_arguments(self):
        self.parser.add_argument(
            '--pretty-out', action='store_true',
            default=os.isatty(sys.stdout.fileno()),
            help='Prettify output for CLI use'
        )
        super(InlinePlayground, self).add_arguments()

    def process_unit(self, unit):

        def previous_not_trivia(token):
            ret = token
            while ret.is_trivia:
                ret = ret.previous
            return ret

        for tok in unit.iter_tokens():
            if tok.kind == 'Comment' and tok.text.startswith('--%'):
                expr_text = tok.text[3:].strip()
                current_node = unit.root.lookup(
                    previous_not_trivia(tok).sloc_range.start
                )
                print("Eval '{}' on node {}".format(
                    col(expr_text, YELLOW),
                    col(current_node.entity_repr, YELLOW)
                ))
                try:
                    value = eval(
                        expr_text, None,
                        {'lal': lal, 'node': current_node}
                    )
                except lal.PropertyError as pe:
                    print('Exception: {}'.format(
                        ' '.join(str(a) for a in pe.args)
                    ))
                else:
                    value_prefix = 'Result: '
                    # Post-process the result of pprint.pformat so that
                    # non-ASCII or non-printable characters are escaped.
                    indent = '\n' + len(value_prefix) * ' '
                    value_repr = "".join(
                        c if (32 <= ord(c) <= 127
                              or c == '\n') else ascii(c)[1:-1]
                        for c in (pprint.pformat(value)
                                  .replace('\n', indent))
                    )
                    print(value_prefix + col(value_repr, YELLOW))
                print('')

        print('')


if __name__ == '__main__':
    InlinePlayground.run()
