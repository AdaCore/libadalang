#! /usr/bin/env python

import argparse

import libadalang as lal

parser = argparse.ArgumentParser(
    description='Detect comparison and arithmetic operands which are'
                ' syntactically identical in the input Ada sources.')
parser.add_argument('files', help='The files to analyze',
                    type=str, nargs='+', metavar='F')

def same_tokens(left, right):
    if len(left) != len(right):
        return False
    for le, ri in zip(left, right):
        if le.kind != ri.kind or le.text != ri.text:
            return False
    return True

def has_same_operands(binop):
    return same_tokens(list(binop.f_left.tokens), list(binop.f_right.tokens))

def interesting_oper(op):
    return not isinstance(op, (lal.OpMult, lal.OpPlus, lal.OpDoubleDot,
                               lal.OpPow, lal.OpConcat))

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
