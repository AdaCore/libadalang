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


def location(node):
    return (node.token_start._sloc_range.start.line,
            node.token_start._sloc_range.start.column)


# Without semantic information, we cannot consider enumeration literals yet
def is_literal(expr):
    return isinstance(expr, (lal.CharLiteral,
                             lal.StringLiteral,
                             lal.CharLiteral,
                             lal.IntLiteral,
                             lal.NullLiteral))


def list_left_unequal_operands(binop):
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
        if isinstance(expr, lal.BinOp) and type(expr.f_op) is type(op):
            return (list_sub_operands(expr.f_left, op)
                    + list_sub_operands(expr.f_right, op))

        elif (isinstance(expr, lal.BinOp)
              and isinstance(expr.f_op, lal.OpNeq)
              and is_literal(expr.f_right)):
            return [(expr.f_left, expr.f_right)]

        else:
            return []

    op = binop.f_op
    return (list_sub_operands(binop.f_left, op)
            + list_sub_operands(binop.f_right, op))


def tokens_text(node):
    return tuple((t.kind, t.text) for t in node.tokens)


def has_same_operands(expr):
    """
    For a logic relation, checks whether any combination of its sub-operands
    are syntactically equivalent. If a duplicate operand is found, return it.

    :rtype: lal.Expr|None
    """
    ops = {}
    all_ops = list_left_unequal_operands(expr)
    if len(all_ops) > 1:
        for op in all_ops:
            (op_left, op_right) = op
            tokens = tokens_text(op_left)
            if tokens in ops:
                return (op_left, ops[tokens], op_right)
            ops[tokens] = op_right


def same_as_parent(binop):
    """
    Checks whether binop is a BinOp with the same structure as its parent (same
    operator).

    :rtype: bool
    """
    par = binop.parent
    return (isinstance(binop, lal.BinOp)
            and isinstance(par, lal.BinOp)
            and type(binop.f_op) is type(par.f_op))


def interesting_oper(op):
    """
    Check that op is a relational operator, which are the operators that
    interrest us in the context of this script.

    :rtype: bool
    """
    return isinstance(op, (lal.OpOr, lal.OpOrElse))


def do_file(f):
    c = lal.AnalysisContext()
    unit = c.get_from_file(f)

    if unit.root is None:
        print('Could not parse {}:'.format(f))
        for diag in unit.diagnostics:
            print('   {}'.format(diag))
            return

    for binop in unit.root.findall(lal.BinOp):
        if interesting_oper(binop.f_op) and not same_as_parent(binop):
            res = has_same_operands(binop)
            if res is not None:
                op, fst_val, snd_val = res
                line, col = location(op)
                print('{}:{}:{}: expression is always true,'
                      ' "{}" is always different from {} or {}'.format(
                          f, line, col, op.text, fst_val.text, snd_val.text))


def main(args):
    for f in args.files:
        do_file(f)


if __name__ == '__main__':
    main(parser.parse_args())
