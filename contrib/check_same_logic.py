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


def list_operands(binop):
    """
    List all the sub-operands of `binop`, as long as they have the same
    operator as `binop`.

    :type binop: lal.BinOp
    """

    def list_sub_operands(expr, op):
        """
        Accumulate sub-operands of `expr`, provided `expr` is a binary operator
        that has `op` as an operator.

        :type expr: lal.Expr
        :type op: lal.Op
        """
        if expr.is_a(lal.BinOp) and type(expr.f_op) is type(op):
            return (list_sub_operands(expr.f_left, op)
                    + list_sub_operands(expr.f_right, op))
        else:
            return [expr]

    op = binop.f_op
    return (list_sub_operands(binop.f_left, op)
            + list_sub_operands(binop.f_right, op))


def is_bool_literal(expr):
    """
    Predicate to check whether `expr` is a boolean literal.
    """
    return expr.is_a(lal.Identifier) and expr.text.lower() in ['true', 'false']


def has_same_operands(expr):
    """
    For a logic relation, checks whether any combination of its sub-operands
    are syntactically equivalent. If a duplicate operand is found, return it.

    :rtype: lal.Expr|None
    """
    ops = set()
    for op in list_operands(expr):
        tokens = tuple((t.kind, t.text) for t in op.tokens)
        if tokens in ops:
            return op
        ops.add(tokens)


def same_as_parent(binop):
    """
    Checks whether binop is a BinOp with the same structure as its parent (same
    operator).

    :rtype: bool
    """
    par = binop.parent
    return (binop.is_a(lal.BinOp)
            and par.is_a(lal.BinOp)
            and type(binop.f_op) is type(par.f_op))


def interesting_oper(op):
    """
    Check that op is a relational operator, which are the operators that
    interrest us in the context of this script.

    :rtype: bool
    """
    return op.is_a(lal.OpAnd, lal.OpOr, lal.OpAndThen, lal.OpOrElse, lal.OpXor)


def do_file(f):
    print f
    c = lal.AnalysisContext()
    unit = c.get_from_file(f)

    if unit.root is None:
        print 'Could not parse {}:'.format(f)
        for diag in unit.diagnostics:
            print '   {}'.format(diag)
            return

    for b in unit.root.findall(lambda e: e.is_a(lal.BinOp)):
        if interesting_oper(b.f_op) and not same_as_parent(b):
            oper = has_same_operands(b)
            if oper:
                print 'Same operand {} for {} in {}'.format(oper, b, f)


def main(args):
    for f in args.files:
        do_file(f)


if __name__ == '__main__':
    main(parser.parse_args())
