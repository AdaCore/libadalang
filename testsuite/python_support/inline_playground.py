#!/usr/bin/env python

import os
import pprint
import re
import sys
from typing import Optional

import libadalang as lal


desc = """
Go over Ada files given as argument, processing any lines starting with --% and
containing a python expression starting with $. For those lines, eval the
expression in a context containing the following variables:

- `node` is the ast node immediately preceding the comment.
- `lal` is the libadalang module.
"""


YELLOW = '\033[33m'

assign_expr_re = re.compile(
    r"\s*"
    r"(?P<identifier>[a-zA-Z_][a-zA-Z0-9_]*)"
    r"\s*"
    r"="
    r"(?P<expr>.*)"
)


def col(msg, color):
    return msg if not sys.stdout.isatty() else color + msg + '\033[0m'


class InlinePlayground(lal.App):

    def main(self):
        self._output_empty = True
        """
        Track whether we have written to the output. Used to emit line breaks
        when appropriate (see the ``prepare_output`` method).
        """

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
        self.parser.add_argument(
            '--charset', default=None,
            help='Charset to use for source decoding'
        )
        self.parser.add_argument(
            '--ignore-parsing-errors', action='store_true',
            help='Do not print parsing errors',
        )
        super(InlinePlayground, self).add_arguments()

    def on_parsing_errors(self, unit: lal.AnalysisUnit) -> None:
        if not self.args.ignore_parsing_errors:
            super().on_parsing_errors(unit)

    def prepare_output(self) -> None:
        if self._output_empty:
            self._output_empty = False
        else:
            print("")

    def process_unit(self, unit):

        def previous_not_trivia(token):
            ret = token
            while ret.is_trivia:
                ret = ret.previous
            return ret

        # Last node for which we evaluated an expression, so that we print
        # "Working on node X" only once per node.
        last_node = None

        # Environment for assignment commands
        local_env = {"lal": lal}

        for tok in unit.iter_tokens():
            if tok.kind == 'Comment' and tok.text.startswith('--%'):
                cmd = tok.text[3:]

                target: Optional[str] = None
                expr: str

                # If the command starts with "<identifier> =", then it's an
                # assignment. Otherwise, we just have an expression to
                # evaluate.
                m = assign_expr_re.match(cmd)
                if m is not None:
                    target = m.group("identifier")
                    expr = m.group("expr")
                else:
                    expr = cmd

                expr = expr.strip()

                # Lookup the node on which to evaluate the expression, emit a
                # heading if the node changed.
                current_node = unit.root.lookup(
                    previous_not_trivia(tok).sloc_range.start
                )
                if current_node != last_node:
                    def heading(with_colors):
                        node_repr = str(current_node)
                        if with_colors:
                            node_repr = col(node_repr, YELLOW)
                        return f"Working on node {node_repr}"
                    self.prepare_output()
                    print(heading(with_colors=True))
                    print("=" * len(heading(with_colors=False)))
                last_node = current_node

                self.prepare_output()
                if target:
                    print("Set '{}' to '{}'".format(
                        col(target, YELLOW), col(expr, YELLOW)
                    ))
                else:
                    print("Eval '{}'".format(col(expr, YELLOW)))

                # Evaluate the expression and print the result. First, update
                # the environment to give access to the current node to the
                # expression to evaluate.
                local_env["node"] = current_node
                value = None
                try:
                    value = eval(expr, None, local_env)
                except (lal.PropertyError, lal.PreconditionFailure) as ex:
                    print('Exception: {}'.format(
                        ' '.join(str(a) for a in ex.args)
                    ))
                else:
                    # Post-process the result of pprint.pformat so that
                    # non-ASCII or non-printable characters are escaped.
                    value_prefix = 'Result: '
                    indent = '\n' + len(value_prefix) * ' '
                    value_repr = "".join(
                        c if (32 <= ord(c) <= 127
                              or c == '\n') else ascii(c)[1:-1]
                        for c in (pprint.pformat(value)
                                  .replace('\n', indent))
                    )
                    print(value_prefix + col(value_repr, YELLOW))

                # If this was an assignment, do it now for the next expressions
                if target:
                    local_env[target] = value


if __name__ == '__main__':
    InlinePlayground.run()
