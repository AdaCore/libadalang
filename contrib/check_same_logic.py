#! /usr/bin/env python

import libadalang as lal
import argparse
import itertools

def same_tokens(left, right):
    if len(left) != len(right):
        return False
    return all(
        map(zip(left, right), 
            lambda le, ri: le.kind == ri.kind and le.text == ri.text)
    )

def list_sub_operands(expr, op):
    if isinstance(expr,lal.BinOp) and expr.f_op == op:
        return list_sub_operands(expr.f_left, op) + list_sub_operands(expr.f_right, op)
    else:
        return [expr]

def list_operands(expr):
    op = expr.f_op
    return list_sub_operands(expr.f_left, op) + list_sub_operands(expr.f_right, op)

def is_true_or_false(expr):
    return isinstance(expr,lal.Identifier) and expr.text.lower() in ['true', 'false']

def has_same_operands(expr):
    opers = list_operands(expr)
    toks = [list(op.tokens) for op in opers]
    for ((oper1,toks1),(oper2,toks2)) in itertools.combinations(zip(opers,toks),r=2):
        if same_tokens(toks1, toks2) and not is_true_or_false(oper1):
            return (True,oper1)
    return (False,expr)

def same_as_parent(expr):
    par = expr.parent
    return isinstance(expr,lal.BinOp) and isinstance(par,lal.BinOp) and expr.f_op == par.f_op

def interesting_oper(op):
    return op in ['and', 'or', 'or_else', 'and_then', 'xor']

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
            if interesting_oper(b.f_op) and not same_as_parent(b):
                (found,oper) = has_same_operands(b)
                if found:
                    print "Same operand", str(oper), "for", str(b), "in", file
    except Exception, e:
        print "Analyzing file failed with exception {}".format(e)
