#! /usr/bin/env python

import libadalang as lal
import argparse

def same_tokens(left,right):
    if len(left) != len(right):
        return False
    for le,ri in zip(left,right):
        if le.kind != ri.kind or le.text != ri.text:
            return False
    return True

def has_same_operands(binop):
    return same_tokens(binop.f_left.tokens, binop.f_right.tokens)

def interesting_oper(op):
    return op != 'mult' and op != 'plus' and op != 'ellipsis' and op != 'pow' and op != 'bin_and'

parser = argparse.ArgumentParser()
parser.add_argument("files", help="The files to analyze",
                    type=str, nargs='+', metavar='F')
args = parser.parse_args()

c = lal.AnalysisContext('utf-8')
for file in args.files:
    try:
        unit = c.get_from_file(file)
        if unit.root is None:
            continue
        binops = unit.root.findall(lambda e: isinstance(e,lal.BinOp))

        for b in binops:
            if interesting_oper(b.f_op) and has_same_operands(b):
                print "Same operands for", str(b), "in", file
    except Exception, e:
        print "Analyzing file failed with exception {}".format(e)
