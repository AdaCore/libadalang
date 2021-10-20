#! /usr/bin/env python

"""
This script will detect a test for nullity of a pointer variable that is
dominated by a dereference of the same variable, without intervening assignment
to the variable.
"""

import argparse
import libadalang as lal


parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('files', help='The files to analyze',
                    type=str, nargs='+', metavar='F')


def location(node):
    return (node.token_start.sloc_range.start.line,
            node.token_start.sloc_range.start.column)


def is_equality_operator(op, polarity):
    """
    Check that op is an equality or disequality operator.

    :param polarity: Boolean which is True for OpEq and False for OpNeq.
    :rtype: bool
    """
    if polarity:
        return isinstance(op, lal.OpEq)
    else:
        return isinstance(op, lal.OpNeq)


def get_nullity_test(expr, polarity):
    """
    Detect if expr is a test that an object is null or not null, and in that
    case return the object being tested.

    We make no attempt to detect if this is really an object or an expression.

    :param polarity: Boolean which is True to indicate equality to null should
                     be detected, and False to indicate disequality to null
                     should be detected.
    :rtype: lal.Expr
    """
    if (isinstance(expr, lal.BinOp) and
            is_equality_operator(expr.f_op, polarity)):
        if isinstance(expr.f_left, lal.NullLiteral):
            return expr.f_right
        if isinstance(expr.f_right, lal.NullLiteral):
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
    if isinstance(expr, (lal.ExplicitDeref, lal.DottedName)):
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
    if isinstance(expr, lal.AssignStmt):
        return expr.f_dest
    return None


def explore(f, subp):
    """
    Explore the content of a subprogram body (which could be also the body of
    an expression function), and detect if an object is dereferenced after
    being tested for equality with null, without any possible assignment to
    the object in between.

    Issue a message in that case.

    :rtype: none?
    """
    def remove_assign(node, nulls):
        var = get_assignment(node)
        if var is not None and var.text in nulls:
            del nulls[var.text]

    def add_nulls(node, nulls, polarity):
        var = get_nullity_test(node, polarity)
        if var is not None:
            nulls[var.text] = var

    def detect_dereference(node, nulls):
        var = get_dereference(node)
        if var is not None and var.text in nulls:
            fst_line, fst_col = location(nulls[var.text])
            snd_line, snd_col = location(node)
            print('{}:{}:{}: dereference of null value after test at line'
                  ' {}'.format(f, snd_line, snd_col, fst_line))

    def traverse_branch(node, nulls, cond=None, neg_cond=None):
        """
        Sub traversal procedure, called by the main traversal procedure
        traverse on branches in the control flow.
        """
        if node is None:
            return

        # Copy the dictionary of objects dereferenced, as the objects that are
        # null on a path should not be considered as such when paths join,
        # e.g. after the if-statement.
        branch_nulls = nulls.copy()

        if cond:
            collect_nulls(cond, branch_nulls, {}, result=True)
        if neg_cond:
            collect_nulls(neg_cond, branch_nulls, {}, result=False)

        # Call traverse recursively
        traverse(node, branch_nulls)

        # Remove those variables which have been redefined in the branch, which
        # we detect by checking whether they are still in the objects known to
        # be null for the branch or not.
        for k in nulls.keys():
            if k not in branch_nulls:
                del nulls[k]

    def collect_nulls(node, nulls, notnulls, result):
        """
        Collect all the tests for nullity and non-nullity of objects that are
        guaranteed to hold when `node` evaluates to `result`.

        :type node: lal.Expr
        :type result: Boolean
        """
        # Add the objects tested against null in node to the dictionary nulls
        add_nulls(node, nulls, polarity=result)
        add_nulls(node, notnulls, polarity=not result)

        # Call collect_nulls recursively on sub-nodes
        if isinstance(node, lal.UnOp) and isinstance(node.f_op, lal.OpNot):
            collect_nulls(node.f_expr, nulls, notnulls, not result)

        elif (isinstance(node, lal.BinOp)
              and isinstance(node.f_op, (lal.OpAnd, lal.OpAndThen))):
            if result:
                collect_nulls(node.f_left, nulls, notnulls, result)
                collect_nulls(node.f_right, nulls, notnulls, result)
            else:
                pass  # no guarantees if A and B evaluates to False

        elif (isinstance(node, lal.BinOp)
              and isinstance(node.f_op, (lal.OpOr, lal.OpOrElse))):
            if not result:
                collect_nulls(node.f_left, nulls, notnulls, result)
                collect_nulls(node.f_right, nulls, notnulls, result)
            else:
                pass  # no guarantees if A or B evaluates to True

        # Always filter out nulls by notnulls. For example we may have assigned
        # an object to null, then performed some call that is not taken into
        # account by the checker, then a test that the object is not null.
        # The test should be enough to consider the object not null.
        for k in notnulls.keys():
            if k in nulls:
                del nulls[k]

    def traverse(node, nulls):
        """
        Main recursive traversal procedure.

        :param node: Current node in the AST.
        :param nulls: Dictionary for the objects equal to null on the path,
                      mapping their text to the object node in the AST.
        """
        if node is None:
            return

        # Start by checking for dereference in the context of the null tests
        # found so far on the path. This may issue a message.
        detect_dereference(node, nulls)

        # Call traverse or traverse_branch recursively on sub-nodes
        if isinstance(node, lal.ObjectDecl):
            tdecl = node.f_type_expr.p_designated_type_decl
            if tdecl is not None and tdecl.p_is_access_type():
                if (node.f_default_expr is None or
                        node.f_default_expr.is_a(lal.NullLiteral)):
                    for id in node.f_ids:
                        nulls[id.text] = id

        elif isinstance(node, lal.AssignStmt):
            for sub in node:
                traverse(sub, nulls)
            remove_assign(node, nulls)

            # Consider the case where null is explicitly assigned to an object.
            # Without semantic information, we don't consider yet the case of
            # the declaration of pointer objects without initializer, which are
            # implicitly set to null.
            if isinstance(node.f_expr, lal.NullLiteral):
                nulls[node.f_dest.text] = node.f_dest

        elif isinstance(node, lal.PragmaNode) and node.f_id.text == "Assert":
            for assoc in node.f_args:
                collect_nulls(assoc.f_expr, nulls, {}, result=True)

        elif isinstance(node, lal.IfStmt):
            traverse(node.f_cond_expr, nulls)
            traverse_branch(node.f_then_stmts, nulls, cond=node.f_cond_expr)
            # Do not attempt yet to propagate conditions of elsif parts
            for sub in node.f_alternatives:
                traverse_branch(sub, nulls, neg_cond=node.f_cond_expr)
            traverse_branch(node.f_else_stmts, nulls,
                            neg_cond=node.f_cond_expr)

        elif isinstance(node, lal.IfExpr):
            traverse(node.f_cond_expr, nulls)
            traverse_branch(node.f_then_expr, nulls, cond=node.f_cond_expr)
            # Do not attempt yet to propagate conditions of elsif parts
            for sub in node.f_elsif_list:
                traverse_branch(sub, nulls, neg_cond=node.f_cond_expr)
            traverse_branch(node.f_else_expr, nulls, neg_cond=node.f_cond_expr)

        elif isinstance(node, lal.CaseStmt):
            traverse(node.f_expr, nulls)
            for sub in node.f_alternatives:
                traverse_branch(sub, nulls)

        # Reset null objects inside and after loop, as control may come back
        # from the loop body after variables may be reassigned in the loop.
        elif isinstance(node, lal.LoopStmt):
            traverse(node.f_spec, nulls)
            traverse_branch(node.f_stmts, {})
            nulls.clear()

        elif (isinstance(node, lal.BinOp) and
                isinstance(node.f_op, lal.OpAndThen)):
            traverse(node.f_left, nulls)
            traverse_branch(node.f_right, nulls, cond=node.f_left)

        elif (isinstance(node, lal.BinOp) and
                isinstance(node.f_op, lal.OpOrElse)):
            traverse(node.f_left, nulls)
            traverse_branch(node.f_right, nulls, neg_cond=node.f_left)

        # Reset null objects for exception handler, as control may come from
        # many sources.
        elif isinstance(node, lal.ExceptionHandler):
            traverse(node.f_stmts, {})

        # Ignore local subprograms and packages when exploring the enclosing
        # subprogram body.
        elif isinstance(node, (lal.SubpBody,
                               lal.PackageDecl,
                               lal.GenericPackageDecl,
                               lal.PackageBody,
                               lal.ExprFunction)):
            pass

        else:
            for sub in node:
                traverse(sub, nulls)

    # Skip the toplevel subprogram body so that it is not confused with a
    # local one.
    def traverse_subp_body(node, nulls):
        for sub in node:
            traverse(sub, nulls)

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
