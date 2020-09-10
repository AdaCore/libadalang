#! /usr/bin/env python

"""
This checker will detect a useless assignments to local variables of two kinds:
(1) the local variable is reassigned with no possible read since the assignment
(2) the subprogram returns before the variable is read.

In the first case, the reassignment must be at the same scope level, or in a
scope above, the initial assignment. So the following useless assignment will
be detected::

   if Cond then
      X := 0;
   end if;
   X := 1;

but not this one::

   X := 0;
   if Cond then
      X := 1;
   else
      X := 2;
   end if;

In the second case, the return must be at the same scope level, or in a
scope above, the initial assignment. So the following useless assignment will
be detected::

   if Cond then
      X := 0;
   end if;
   return;

but not this one::

   X := 0;
   if Cond then
      return;
   else
      return;
   end if;

Record paths like X.Y are considered as well, so the following useless
assignment will be detected::

   if Cond then
      X.Y := 0;
   end if;
   X := 1;

as well as this one::

   if Cond then
      X.Y := 0;
   end if;
   X.Y := 1;

"""

import argparse
import libadalang as lal


parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('files', help='The files to analyze',
                    type=str, nargs='+', metavar='F')


def location(node):
    return (node.token_start._sloc_range.start.line,
            node.token_start._sloc_range.start.column)


def get_read(expr):
    """
    Detect if expr is a read of an object, and in that case return the object
    being read.

    Without semantic information, we cannot know if the 'object' is an object
    or a parameterless function call, an array access or a call. We consider
    all these as objects being read.

    :param lal.Expr expr:
    :rtype: lal.Identifier | lal.DottedName | lal.CallExpr | None
    """
    if isinstance(expr, (lal.Identifier, lal.DottedName, lal.CallExpr)):
        return expr
    return None


def is_ignored_name(name):
    return any([prefix for prefix in ("discard", "dummy", "ignore", "res",
                                      "status", "success", "temp", "tmp",
                                      "unref", "unused")
                if name.lower().startswith(prefix)])


def collect_local_vars(node, locvars,
                       no_renaming,
                       no_unreferenced,
                       no_warnings_off,
                       no_address_taken,
                       no_aliased):
    """
    Collect the names of all the local variables declared in the subprogram.

    :param bool no_renaming: True if renamings should be ignored.
    :param bool no_unreferenced: True if variables with pragma Unreferenced
                                 should be ignored.
    :param bool no_warnings_off: True if variables with pragma Warnings Off
                                 should be ignored.
    :param bool no_address_taken: True if variables whose address is specified
                                  through an Address clause or taken through
                                  an Address attribute reference should be
                                  ignored.
    :param bool no_aliased: True if aliased variables should be ignored.
    """
    def remove_local_var(obj):
        if obj.text in locvars:
            del locvars[obj.text]

    if node is None:
        pass

    elif isinstance(node, lal.ObjectDecl):
        if no_renaming and node.f_renaming_clause is not None:
            pass
        # Consider the presence of aspect Address
        elif (no_address_taken and
              node.f_aspects is not None and
              any([aspect.f_id.text == "Address"
                   for aspect in node.f_aspects.f_aspect_assocs])):
            pass
        # Consider that the variable may be marked "aliased"
        elif (no_aliased and
              node.f_has_aliased is not None and
              node.f_has_aliased.p_as_bool):
            pass
        else:
            for var in node.f_ids:
                locvars[var.text] = var

    # Currently we do not look at the aspect version of pragmas Unreferenced
    # and Warnings Off.
    elif isinstance(node, lal.PragmaNode):
        if no_unreferenced and node.f_id.text == "Unreferenced":
            for assoc in node.f_args:
                remove_local_var(assoc.f_expr)
        elif (no_warnings_off and
              node.f_id.text == "Warnings" and
              len(node.f_args) >= 2 and
              node.f_args[0].f_expr.text == "Off"):
            remove_local_var(node.f_args[1].f_expr)

    elif isinstance(node, lal.AttributeRef):
        if no_address_taken and node.f_attribute.text == "Address":
            remove_local_var(node.f_prefix)

    # We should also deal here with Volatile, Atomic and Import'ed variables

    else:
        for sub in node:
            collect_local_vars(sub, locvars,
                               no_renaming,
                               no_unreferenced,
                               no_warnings_off,
                               no_address_taken,
                               no_aliased)


def collect_local_subprograms(node, locsubprograms):
    """
    Collect the names of all the local subprograms declared in the subprogram.
    """
    if node is None:
        pass
    elif (isinstance(node, (lal.SubpSpec, lal.GenericSubpInstantiation)) and
          # Check presence of f_subp_name to rule out the definition of a
          # subprogram access type.
          node.f_subp_name is not None):
        locsubprograms.add(node.f_subp_name.text)
    else:
        for sub in node:
            collect_local_subprograms(sub, locsubprograms)


def is_local_var(node, locvars):
    if isinstance(node, lal.Identifier):
        return node.text in locvars
    elif isinstance(node, lal.DottedName):
        return is_local_var(node.f_prefix, locvars)
    elif isinstance(node, lal.CallExpr):
        return is_local_var(node.f_name, locvars)
    else:
        return False


def is_variable_or_record_path(node):
    if isinstance(node, lal.Identifier):
        return True
    elif isinstance(node, lal.DottedName):
        return is_variable_or_record_path(node.f_prefix)
    else:
        return False


def is_local_subprogram(node, locsubprograms):
    if isinstance(node, lal.Identifier):
        return node.text in locsubprograms
    else:
        return False


def explore(f, locvars, locsubprograms, subp):
    """
    Explore the content of a subprogram body, and detect if an assignment to a
    local variable is useless, either because it is reassigned with no possible
    read since the assignment, or because the subprogram returns before the
    variable is read. In the first case, the reassignment must be at the same
    scope level, or in a scope above, the initial assignment.

    Issue a message in both cases.

    We do this by traversing the AST in reverse order, maintaining two pieces
    of information:
    - the set "reads" of the access paths of local variables read before they
      are assigned.
    - the map "assigns" of the access paths of local variables assigned before
      they are read.

    For the above computation, an assignment to X.Y.Z counts also as a read of
    X and a read of X.Y.

    Subprogram parameters are considered as special local variables, that
    should be considered as read on return.

    Currently only record paths like X.Y.Z are considered, not array paths like
    X(Y), as there are too many false alarms with array paths without semantic
    information. There are also some false alarms related to some syntactic
    record path including access dereferences, which could be removed in the
    future with semantic information. In particular, we only detect the second
    type of useless assignment (before return) on whole variables, as there
    are too many false alarms otherwise.
    """
    # The initial set of reads is the set of subprogram parameters. This
    # includes the OUT and IN OUT parameters which can be read after the
    # subprogram returns.
    params = set([param.f_ids.text
                  for param in subp.f_subp_spec.p_params])

    def remove_read(node, assigns, reads):
        obj = get_read(node)
        if obj is not None:
            if (is_local_var(obj, locvars) and
                    is_variable_or_record_path(obj)):
                reads.add(obj.text)
                if obj.text in assigns:
                    del assigns[obj.text]
            # If not itself the prefix of a record access path like X.Y.Z,
            # consider that a read of X.Y after and before successive
            # assignments to X.Y.Z is enough to validate them.
            if not isinstance(obj.parent, lal.DottedName):
                for path in list(assigns.keys()):
                    if path.startswith(obj.text):
                        del assigns[path]

    def detect_reassign(node, assigns, reads):
        if isinstance(node, lal.AssignStmt):
            obj = node.f_dest
            # Only report issues on record paths
            if is_local_var(obj, locvars) and is_variable_or_record_path(obj):
                if obj.text in assigns:
                    fst_line, fst_col = location(obj)
                    snd_line, snd_col = location(assigns[obj.text])
                    print('{}:{}:{}: useless assignment,'
                          ' {} reassigned at line {}'.format(
                              f, fst_line, fst_col, obj.text, snd_line))

                # Without semantic information, we cannot know if assignment to
                # X.C is through a pointer X to memory. So currently only
                # detect useless assignment before return on whole variables.
                elif (isinstance(obj, lal.Identifier) and
                        obj.text not in reads):
                    fst_line, fst_col = location(obj)
                    print('{}:{}:{}: useless assignment,'
                          ' {} not read before return'.format(
                              f, fst_line, fst_col, obj.text))

    def declare_assign(node, assigns):
        if is_local_var(node, locvars):
            assigns[node.text] = node

    def traverse_branch(node,
                        init_assigns, update_assigns,
                        init_reads, update_reads):
        """
        Sub traversal procedure, called by the main traversal procedure
        traverse on branches in the control flow.

        :param lal.AdaNode node: Node for a branch in the AST.
        :param map init_assigns: Initial value of the assignment map.
        :param map update_assigns: Assignment map to update as a result of the
                                   branch traversal.
        :param set init_reads: Initial value of the set of reads.
        :param set update_reads: Set of reads to update as a result of the
                                 branch traversal.
        """
        if node is None:
            return

        # Copy the dictionary of objects assigned, as the objects that are
        # assigned on a path should not be considered as such when paths join,
        # e.g. when moving from a branch of an if-statement to before it.
        branch_assigns = init_assigns.copy()

        # Copy the set of object read so far
        branch_reads = init_reads.copy()

        # Call traverse recursively
        traverse(node, branch_assigns, branch_reads)

        # Remove those variables which have been read in the branch, which
        # we detect by checking whether they are still in the objects known to
        # be assigned for the branch or not.
        for k in list(update_assigns.keys()):
            if k not in branch_assigns:
                del update_assigns[k]

        # Join the object read on the branch
        update_reads |= branch_reads

    def traverse(node, assigns, reads):
        """
        Main recursive traversal procedure. It traverses the AST in reverse
        order.

        :param lal.AdaNode node: Current node in the AST.
        :param map assigns: Dictionary for the local objects assigned on the
            path, mapping their text to the object node in the AST.
        :param set reads: Set for the local objects read on the path.
        """
        if node is None:
            return

        # Start by checking for an assignment that will be always reassigned
        # without intervening read. This may issue a message.
        if isinstance(node, lal.AssignStmt):
            detect_reassign(node, assigns, reads)
            # Store new assignment
            declare_assign(node.f_dest, assigns)
            # Call traverse recursively, skipping the object assigned itself
            traverse(node.f_expr, assigns, reads)
            if isinstance(node.f_dest, lal.Identifier):
                pass
            else:
                for sub in node.f_dest:
                    traverse(sub, assigns, reads)

        # Call traverse or traverse_branch recursively on sub-nodes
        elif isinstance(node, lal.IfStmt):
            # Save initial version of assigns and reads
            init_assigns = assigns.copy()
            init_reads = reads.copy()
            # Deal with all branches, using the "init" versions and updating
            # the versions propagated.
            traverse_branch(node.f_else_stmts, init_assigns, assigns,
                            init_reads, reads)
            for sub in node.f_alternatives:
                traverse_branch(sub, init_assigns, assigns, init_reads, reads)
            traverse_branch(node.f_then_stmts, init_assigns, assigns,
                            init_reads, reads)
            traverse(node.f_cond_expr, assigns, reads)

        elif isinstance(node, lal.CaseStmt):
            # Save initial version of assigns and reads
            init_assigns = assigns.copy()
            init_reads = reads.copy()
            # Deal with all branches, using the "init" versions and updating
            # the versions propagated.
            for sub in node.f_alternatives:
                traverse_branch(sub, init_assigns, assigns, init_reads, reads)
            traverse(node.f_expr, assigns, reads)

        # Reset assigns objects inside and before loop, as control may come
        # back from the loop body after variables may be read in the loop. Also
        # consider all local variables as possibly read.
        elif isinstance(node, lal.BaseLoopStmt):
            reads |= set(locvars.keys())
            traverse_branch(node.f_stmts, {}, {}, reads, reads)
            traverse(node.f_spec, assigns, reads)
            assigns.clear()

        # Reset assigns objects for exception handler, as control may come from
        # many sources, when exception is rethrown. Keep read variables.
        elif isinstance(node, lal.ExceptionHandler):
            traverse_branch(node.f_stmts, {}, {}, reads, reads)

        # Control jumps after exit, goto or raise, hence do not consider any of
        # the assignments occurring in the code after these.
        elif isinstance(node, (lal.ExitStmt,
                               lal.GotoStmt,
                               lal.RaiseStmt)):
            assigns.clear()
            reads |= set(locvars.keys())
            for sub in node:
                traverse(sub, assigns, reads)

        # Control jumps after return hence do not consider any of the
        # assignments occurring in the code after it. Also reset the set of
        # local variables read afterwards to the set of parameters for the
        # subprogram.
        elif isinstance(node, (lal.ReturnStmt,
                               lal.ExtendedReturnStmt)):
            assigns.clear()
            reads.clear()
            reads |= params
            for sub in node:
                traverse(sub, assigns, reads)

        # Ignore local subprograms and packages when exploring the enclosing
        # subprogram body.
        elif isinstance(node, (lal.SubpBody,
                               lal.PackageDecl,
                               lal.GenericPackageDecl,
                               lal.PackageBody,
                               lal.ExprFunction)):
            pass

        else:
            remove_read(node, assigns, reads)

            # Reset assigns objects through calls to local subprograms. Also
            # consider all local variables as possibly read through the
            # call. Without semantic information, we don't distinguish calls
            # from identifiers and array access.

            if isinstance(node, lal.Identifier):
                callee = node
            elif isinstance(node, lal.CallStmt):
                callee = node.f_call
            elif isinstance(node, lal.CallExpr):
                callee = node.f_name
            elif (isinstance(node, lal.AttributeRef) and
                  node.f_attribute.text in ("Access",
                                            "Unchecked_Access",
                                            "Unrestricted_Access")):
                callee = node.f_prefix
            else:
                callee = None

            if (callee is not None and
                    is_local_subprogram(callee, locsubprograms)):
                assigns.clear()
                reads |= set(locvars.keys())

            for sub in reversed(node):
                traverse(sub, assigns, reads)

    # Skip the toplevel subprogram body so that it is not confused with a
    # local one.
    def traverse_subp_body(node, assigns, reads):
        for sub in reversed(node):
            traverse(sub, assigns, reads)

    # The initial set of reads is the set of subprogram parameters. This
    # includes the OUT and IN OUT parameters which can be read after the
    # subprogram returns.
    reads = params.copy()
    traverse_subp_body(subp, {}, reads)


def do_file(f):
    c = lal.AnalysisContext()
    unit = c.get_from_file(f)
    if unit.root is None:
        print('Could not parse {}:'.format(f))
        for diag in unit.diagnostics:
            print('   {}'.format(diag))
            return

    for subp in unit.root.findall(lal.SubpBody):
        # Collect local variables for which useless assignment will be
        # detected.
        locvars = {}
        collect_local_vars(subp, locvars,
                           no_renaming=True,
                           no_unreferenced=True,
                           no_warnings_off=True,
                           no_address_taken=True,
                           no_aliased=True)
        # Filter out variables whose name indicates they are not used, or an
        # indicator of success of a command with side-effect, which may not
        # always be used.
        for name in list(locvars.keys()):
            if is_ignored_name(name):
                del locvars[name]
        # Collect local subprograms which may update the value of local
        # variables.
        locsubprograms = set()
        collect_local_subprograms(subp, locsubprograms)
        # Main traversal function
        explore(f, locvars, locsubprograms, subp)


def main(args):
    for f in args.files:
        do_file(f)


if __name__ == '__main__':
    main(parser.parse_args())
