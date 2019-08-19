#! /usr/bin/env python

"""
This script will detect syntactically identical expressions which are chained
together in a chain of logical operators in the input Ada sources.
"""

from __future__ import (absolute_import, division, print_function)

import argparse
import libadalang as lal


parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('files', help='The files to analyze',
                    type=str, nargs='+', metavar='F')


def location(node):
    return (node.token_start._sloc_range.start.line,
            node.token_start._sloc_range.start.column)


def list_tests(ifnode):
    """
    List all the tests of `ifnode`.

    :type ifnode: lal.IfStmt|lal.IfExpr
    """
    return [ifnode.f_cond_expr] + [
        f.f_cond_expr for f in ifnode.f_alternatives
    ]


def tokens_text(node):
    return tuple((t.kind, t.text) for t in node.tokens)


def one_liner(node):
    return ''.join(map(lambda t: t.text, node.tokens))


def has_same_tests(expr):
    """
    For an if-statement or an if-expression, checks whether any combination of
    its tests are syntactically equivalent. If duplicate operands are found,
    return them.

    :rtype: lal.Expr|None
    """
    tests = {}
    all_tests = list_tests(expr)
    if len(all_tests) > 1:
        for test in all_tests:
            tokens = tokens_text(test)
            if tokens in tests:
                return (tests[tokens], test)
            tests[tokens] = test


def do_file(f):
    c = lal.AnalysisContext()
    unit = c.get_from_file(f)

    if unit.root is None:
        print('Could not parse {}:'.format(f))
        for diag in unit.diagnostics:
            print('   {}'.format(diag))
            return

    for ifnode in unit.root.findall((lal.IfStmt, lal.IfExpr)):
        res = has_same_tests(ifnode)
        if res is not None:
            fst_test, snd_test = res
            fst_line, fst_col = location(fst_test)
            snd_line, snd_col = location(snd_test)
            print('{}:{}:{}: duplicate test with line {}'.format(
                f, snd_line, snd_col, fst_line
            ))


def main(args):
    for f in args.files:
        do_file(f)


if __name__ == '__main__':
    main(parser.parse_args())
