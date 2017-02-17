#! /usr/bin/env python

"""
This script will detect comparison and arithmetic operations that have operands
which are syntactically identical in the input Ada sources.
"""

import argparse

import libadalang as lal

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument(
    'files', help='A file to analyze', type=str, nargs='+', metavar='file'
)


def same_tokens(left, right):
    """
    Returns whether left and right contain tokens that are structurally
    equivalent with regards to kind and contained text.

    :rtype: bool
    """
    return len(left) == len(right) and all(
        le.kind == ri.kind and le.text == ri.text
        for le, ri in zip(left, right)
    )


def has_same_operands(binop):
    """
    Checks whether binop has the same operands syntactically.

    :type binop: lal.BinOp
    :rtype: bool
    """
    return same_tokens(list(binop.f_left.tokens), list(binop.f_right.tokens))


def interesting_oper(op):
    """
    Predicate that returns whether op is an operator that is interesting in the
    context of this script.

    :rtype: bool
    """
    return not op.is_a(lal.OpMult, lal.OpPlus, lal.OpDoubleDot,
                       lal.OpPow, lal.OpConcat)


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
            for b in unit.root.findall(lal.BinOp):
                if interesting_oper(b.f_op) and has_same_operands(b):
                    print 'Same operands for {} in {}'.format(b, f)
        except Exception, e:
            print 'Analyzing {} failed with exception: {}: {}'.format(
                f, type(e).__name__, e)


if __name__ == '__main__':
    main(parser.parse_args())
