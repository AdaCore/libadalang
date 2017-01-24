#! /usr/bin/env python

import argparse
from itertools import combinations

import libadalang as lal

parser = argparse.ArgumentParser(
    description='Detect syntactically identical expressions which are chained'
                ' together in a chain of logical operators.')
parser.add_argument('files', help='The files to analyze',
                    type=str, nargs='+', metavar='F')

def same_tokens(left, right):
    if len(left) != len(right):
        return False
    for le, ri in zip(left, right):
        if le.kind != ri.kind or le.text != ri.text:
            return False
    return True

def list_sub_operands(expr, op):
    if isinstance(expr, lal.BinOp) and type(expr.f_op) == type(op):
        return (list_sub_operands(expr.f_left, op)
                + list_sub_operands(expr.f_right, op))
    else:
        return [expr]

def list_operands(expr):
    op = expr.f_op
    return (list_sub_operands(expr.f_left, op)
            + list_sub_operands(expr.f_right, op))

def is_true_or_false(expr):
    return (isinstance(expr, lal.Identifier)
            and expr.text.lower() in ['true', 'false'])

def has_same_operands(expr):
    opers = list_operands(expr)
    toks = [list(op.tokens) for op in opers]
    for (oper1, toks1), (oper2, toks2) in combinations(zip(opers, toks), r=2):
        if same_tokens(toks1, toks2) and not is_true_or_false(oper1):
            return (True, oper1)
    return (False, expr)

def same_as_parent(expr):
    par = expr.parent
    return (isinstance(expr, lal.BinOp)
            and isinstance(par, lal.BinOp)
            and type(expr.f_op) == type(par.f_op))

def interesting_oper(op):
    return isinstance(op, (lal.OpAnd, lal.OpOr, lal.OpAndThen, lal.OpOrElse,
                           lal.OpXor))

def main(args):
    c = lal.AnalysisContext('utf-8')
    for f in args.files:
        try:
            unit = c.get_from_file(f)
            if unit.root is None:
                print 'Could not parse {}:'.format(f)
                for diag in unit.diagnostics:
                    print '   {}'.format(diag)
                continue

            for b in unit.root.findall(lambda e: isinstance(e, lal.BinOp)):
                if interesting_oper(b.f_op) and not same_as_parent(b):
                    found, oper = has_same_operands(b)
                    if found:
                        print 'Same operand {} for {} in {}'.format(oper, b, f)
        except Exception, e:
            print 'Analyzing {} failed with exception: {}: {}'.format(
                f, type(e).__name__, e)

if __name__ == '__main__':
    main(parser.parse_args())
