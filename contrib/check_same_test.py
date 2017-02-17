#! /usr/bin/env python

"""
This script will detect syntactically identical expressions which are chained
together in a chain of logical operators in the input Ada sources.
"""

import argparse
import libadalang as lal

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('files', help='The files to analyze',
                    type=str, nargs='+', metavar='F')


def list_tests(ifnode):
    """
    List all the tests of `ifnode`.

    :type ifnode: lal.IfStmt lal.IfExpr
    """

    if isinstance(ifnode, lal.IfStmt):
        return [ifnode.f_condition] + [f.f_expr for f in ifnode.f_alternatives]
    else:
        return [ifnode.f_cond_expr] + [f.f_cond_expr for f in ifnode.f_elsif_list]


def has_same_tests(expr):
    """
    For an if-statement or an if-expression, checks whether any combination of
    its tests are syntactically equivalent. If a duplicate operand is found,
    return it.

    :rtype: lal.Expr|None
    """
    tests = set()
    for test in list_tests(expr):
        tokens = tuple((t.kind, t.text) for t in test.tokens)
        if tokens in tests:
            return test
        tests.add(tokens)


def do_file(f):
    print f
    c = lal.AnalysisContext()
    unit = c.get_from_file(f)

    if unit.root is None:
        print 'Could not parse {}:'.format(f)
        for diag in unit.diagnostics:
            print '   {}'.format(diag)
            return

    for b in unit.root.findall(lambda e: e.is_a(lal.IfStmt, lal.IfExpr)):
        oper = has_same_tests(b)
        if oper:
            print 'Same test {} for {} in {}'.format(oper, b, f)


def main(args):
    for f in args.files:
        do_file(f)

if __name__ == '__main__':
    main(parser.parse_args())
