#! /usr/bin/env python

"""
This script will detect a test for nullity of a pointer variable that is
dominated by a dereference of the same variable, without intervening assignment
to the variable.
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


def is_equality_operator(op):
    """
    Check that op is an equality or disequality operator.

    :rtype: bool
    """
    return op.is_a(lal.OpEq, lal.OpNeq)


def get_nullity_test(expr):
    """
    Detect if expr is a test that an object is null or not null, and in that
    case return the object being tested.

    We make no attempt to detect if this is really an object or an expression.

    :rtype: expr?
    """
    if expr.is_a(lal.BinOp) and is_equality_operator(expr.f_op):
        if expr.f_left.is_a(lal.NullLiteral):
            return expr.f_right
        if expr.f_right.is_a(lal.NullLiteral):
            return expr.f_left
    return None


def get_dereference(expr):
    """
    Detect if expr is a dereference of a pointer object, and in that case
    return the object being dereferenced.

    Without semantic information, we cannot know if the 'object' is an object
    or a package, and even if it is an object, if it is of access type. We
    consider all these as objects being referenced.

    :rtype: expr?
    """
    if expr.is_a(lal.ExplicitDeref, lal.DottedName):
        return expr.f_prefix
    return None


def get_assignment(expr):
    """
    Detect if expr is an assignment of an object, and in that case return
    the object being assigned.

    We make no attempt to detect if this is really an object or an expression.
    Without semantic information, we also cannot know when an object is
    possibly assigned by being passed as OUT or IN OUT parameter in a call. We
    also cannot know if the address of an object is taken.

    We could try to detect when the prefix of an object is assigned, thus
    assigning also the object as a side-effect, but leave it for a future
    version using semantic information.

    :rtype: expr?
    """
    if expr.is_a(lal.AssignStmt):
        return expr.f_dest
    return None


def explore(f, subp):
    """
    Explore the content of a subprogram body (which could be also the body of
    an expression function), and detect if an object is tested for
    (dis)equality with null, after being dereferenced, without any possible
    assignment to the object in between.

    Issue a message in that case.

    :rtype: none?
    """
    def remove_assign(node, derefs):
        var = get_assignment(node)
        if var is not None and var.text in derefs:
            del derefs[var.text]

    def add_derefs(node, derefs):
        var = get_dereference(node)
        if var is not None:
            derefs[var.text] = var

    def detect_nullity(node, derefs):
        var = get_nullity_test(node)
        if var is not None and var.text in derefs:
            fst_line, fst_col = location(derefs[var.text])
            snd_line, snd_col = location(node)
            print('{}:{}:{}: suspicious test of null value after dereference'
                  ' at line {}'.format(f, snd_line, snd_col, fst_line))

    def traverse_branch(node, derefs, loop_test):
        """
        Sub traversal procedure, called by the main traversal procedure
        traverse on branches in the control flow.
        """
        if node is None:
            return

        # Copy the dictionary of objects dereferenced, as the objects
        # dereferenced on a path should not be considered as such when paths
        # join, e.g. after the if-statement.
        branch_derefs = derefs.copy()

        # Call traverse recursively
        traverse(node, branch_derefs, loop_test)

        # Remove those variables which have been redefined in the branch, which
        # we detect by checking whether they are still in the objects
        # dereferenced for the branch or not.
        for k in derefs.keys():
            if k not in branch_derefs:
                del derefs[k]

    def traverse(node, derefs, loop_test):
        """
        Main recursive traversal procedure.

        :param node: Current node in the AST.
        :param derefs: Dictionary for the objects dereferenced on the path,
                       mapping their text to the object node in the AST.
        :param loop_test: Boolean that is True if node is within a loop test.
        """
        if node is None:
            return

        # Start by checking for nullity test in the context of the dereferenced
        # found so far on the path. This may issue a message. Do not test
        # nullity on loop tests, as variable tested may be reassigned in the
        # loop.
        if not loop_test:
            detect_nullity(node, derefs)

        # Add the objects dereferenced in node to the dictionary derefs
        add_derefs(node, derefs)

        # Call traverse or traverse_branch recursively on sub-nodes
        if node.is_a(lal.AssignStmt):
            for sub in node:
                traverse(sub, derefs, loop_test)
            remove_assign(node, derefs)

        elif node.is_a(lal.IfStmt):
            traverse(node.f_cond_expr, derefs, loop_test)
            traverse_branch(node.f_then_stmts, derefs, loop_test)
            for sub in node.f_alternatives:
                traverse_branch(sub, derefs, loop_test)
            traverse_branch(node.f_else_stmts, derefs, loop_test)

        elif node.is_a(lal.IfExpr):
            traverse(node.f_cond_expr, derefs, loop_test)
            traverse_branch(node.f_then_expr, derefs, loop_test)
            for sub in node.f_alternatives:
                traverse_branch(sub, derefs, loop_test)
            traverse_branch(node.f_else_expr, derefs, loop_test)

        elif node.is_a(lal.CaseStmt):
            traverse(node.f_expr, derefs, loop_test)
            for sub in node.f_alternatives:
                traverse_branch(sub, derefs, loop_test)

        elif node.is_a(lal.BaseLoopStmt):
            traverse(node.f_spec, derefs, loop_test=True)
            traverse_branch(node.f_stmts, derefs, loop_test)

        elif node.is_a(lal.BinOp) \
                and node.f_op.is_a(lal.OpAndThen, lal.OpOrElse):
            traverse(node.f_left, derefs, loop_test)
            traverse_branch(node.f_right, derefs, loop_test)

        # Reset dereferences for exception handler, as control may come from
        # many sources.
        elif node.is_a(lal.ExceptionHandler):
            traverse(node.f_stmts, {}, loop_test)

        # Ignore local subprograms and packages when exploring the enclosing
        # subprogram body.
        elif not node.is_a(
            lal.SubpBody, lal.PackageDecl, lal.GenericPackageDecl,
            lal.PackageBody, lal.ExprFunction
        ):
            for sub in node:
                traverse(sub, derefs, loop_test)

    # Skip the toplevel subprogram body so that it is not confused with a
    # local one.
    def traverse_subp_body(node, derefs):
        for sub in node:
            traverse(sub, derefs, loop_test=False)

    traverse_subp_body(subp, {})


def do_file(f):
    c = lal.AnalysisContext()
    unit = c.get_from_file(f)
    if unit.root is None:
        print('Could not parse {}:'.format(f))
        for diag in unit.diagnostics:
            print('   {}'.format(diag))
            return

    for subp in unit.root.findall((lal.SubpBody, lal.ExprFunction)):
        explore(f, subp)


def main(args):
    for f in args.files:
        do_file(f)


if __name__ == '__main__':
    main(parser.parse_args())
