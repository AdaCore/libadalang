#! /usr/bin/env python

"""
This script will detect syntactically identical blocks in an if- or case-
statement or expression. It implements heuristics to avoid flagging valid uses
of code duplication.

Hence no message is issued in the following cases:
- if the duplicated block has less than 10 tokens
- if the duplicated block has fewer tokens than the test for this block
- if the duplicated block is the "else" part in an if-statement or an
  if-expression, and it duplicates a block not directly before the "else"
  part.
"""

import argparse
import libadalang as lal


parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('files', help='The files to analyze',
                    type=str, nargs='+', metavar='F')


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


def have_same_tokens(left, right):
    """
    Returns whether left and right nodes contain tokens that are structurally
    equivalent with regards to kind and contained text.

    :rtype: bool
    """
    return same_tokens(list(left.tokens), list(right.tokens))


def list_blocks(node):
    """
    List all the sub-blocks of `node` that should be considered for duplicate
    checking. This is where we filter blocks based on heuristics.

    :type node: lal.IfStmt lal.IfExpr lal.CaseStmt lal.CaseExpr
    """
    def num_tokens(node):
        return len(list(node.tokens))

    def select_if_block(block, test):
        # Only report blocks of length greater than 10 tokens, and it the
        # length of the test leading to the block is no greater than the length
        # of the block. Otherwise, not sharing the blocks might be better
        # coding style.
        len_test = num_tokens(test)
        return num_tokens(block) > max(10, len_test)

    def select_case_block(block):
        # Only report blocks of length greater than 10 tokens
        return num_tokens(block) > 10

    def last_block_before_else(node):
        """
        Return the last block of code before the else part, in an if-statement
        or an if-expression.
        """
        if isinstance(node, lal.IfStmt):
            if len(node.f_alternatives) == 0:
                return node.f_then_stmts
            else:
                return node.f_alternatives[-1].f_stmts
        else:
            if len(node.f_alternatives) == 0:
                return node.f_then_expr
            else:
                return node.f_alternatives[-1].f_then_expr

    if isinstance(node, lal.IfStmt):
        blocks = []
        if select_if_block(node.f_then_stmts, node.f_cond_expr):
            blocks += [node.f_then_stmts]
        blocks += [sub.f_stmts for sub in node.f_alternatives
                   if select_if_block(sub.f_stmts, sub.f_cond_expr)]
        # Only return the else block if it is the same as the block preceding
        # it. Otherwise, there may be valid reasons for code duplication, that
        # have to do with the order of evaluation of tests in an if-statement.
        if (node.f_else_stmts and
                have_same_tokens(node.f_else_stmts,
                                 last_block_before_else(node))):
            blocks += [node.f_else_stmts]

    elif isinstance(node, lal.IfExpr):
        blocks = []
        if select_if_block(node.f_then_expr, node.f_cond_expr):
            blocks += [node.f_then_expr]
        blocks += [sub.f_then_expr for sub in node.f_alternatives
                   if select_if_block(sub.f_then_expr, sub.f_cond_expr)]
        # Only return the else block if it is the same as the block preceding
        # it. Otherwise, there may be valid reasons for code duplication, that
        # have to do with the order of evaluation of tests in an if-expression.
        if (node.f_else_expr and
                have_same_tokens(node.f_else_expr,
                                 last_block_before_else(node))):
            blocks += [node.f_else_expr]

    elif isinstance(node, lal.CaseStmt):
        blocks = [sub.f_stmts for sub in node.f_alternatives
                  if select_case_block(sub.f_stmts)]

    elif isinstance(node, lal.CaseExpr):
        blocks = [sub.f_expr for sub in node.f_cases
                  if select_case_block(sub.f_expr)]

    else:
        assert False

    return blocks


def location(node):
    return (node.token_start._sloc_range.start.line,
            node.token_start._sloc_range.start.column)


def has_same_blocks(node):
    """
    For an if- or case- statement or expression, checks whether any combination
    of its sub-blocks are syntactically equivalent. If some duplicate operands
    are found, return them.

    :rtype: lal.Expr|None
    """
    blocks = {}
    duplicates = []
    for block in list_blocks(node):
        tokens = tuple((t.kind, t.text) for t in block.tokens)
        if tokens in blocks:
            duplicates.append((blocks[tokens], location(block)))
        else:
            blocks[tokens] = location(block)
    return duplicates


def do_file(f):
    c = lal.AnalysisContext()
    unit = c.get_from_file(f)

    if unit.root is None:
        print('Could not parse {}:'.format(f))
        for diag in unit.diagnostics:
            print('   {}'.format(diag))
            return

    for b in unit.root.findall((lal.IfStmt, lal.IfExpr, lal.CaseStmt,
                                lal.CaseExpr)):
        duplicates = has_same_blocks(b)
        for duplicate in duplicates:
            (fst_line, fst_col), (snd_line, snd_col) = duplicate
            print('{}:{}:{}: duplicate code already found at line {}'.format(
                f, snd_line, snd_col, fst_line
            ))


def main(args):
    for f in args.files:
        do_file(f)


if __name__ == '__main__':
    main(parser.parse_args())
