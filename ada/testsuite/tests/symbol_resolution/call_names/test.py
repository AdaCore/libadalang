# Test the behavior of the p_entities property on various names for CallExpr
# nodes. This test in particular checks the filtering of SubprogramSpec
# depending on the arguments provided to the CallExpr.

import libadalang as lal

import source


with open('foo.adb') as f:
    src_lines = f.readlines()
src_slice = lambda node: source.src_slice(src_lines, node.sloc_range)


ctx = lal.AnalysisContext()
unit = ctx.get_from_file('foo.adb')
unit.populate_lexical_env()


def strip_to_id(call_stmt):
    """
    Strip all layers in "stmt" to get the Identifier for which we want to
    resolve entities.
    """
    expr = call_stmt.f_call
    while not isinstance(expr, lal.Identifier):
        if isinstance(expr, lal.CallExpr):
            expr = expr.f_name
        elif isinstance(expr, lal.Prefix):
            expr = expr.f_suffix
        else:
            assert False, 'Unexpected node: {}'.format(expr)
    return expr


def subp_qualname(subp_spec):
    """
    Return a non-ambiguous name + signature for a SubprogramSpec.
    """
    qual_name = []
    parent = subp_spec
    while parent:
        if isinstance(parent, (lal.PackageBody, lal.PackageDecl)):
            qual_name.append(parent.f_package_name)
        elif isinstance(parent, (lal.SubprogramDecl, lal.SubprogramBody)):
            qual_name.append(parent.f_subp_spec.f_name)
        parent = parent.parent

    return '{}{}'.format(
        '.'.join(n.f_tok.text for n in reversed(qual_name)),
        ' ({})'.format(src_slice(subp_spec.f_params))
        if subp_spec.f_params else ''
    )


# Each subprogram body contains statements that are our test cases
for subp_body in unit.root.finditer(lal.SubprogramBody):
    print('== {} =='.format(subp_body.f_subp_spec.f_name.f_tok.text))

    for stmt in subp_body.f_statements.f_statements:
        # For each test case, print the list of matching entities
        identifier = strip_to_id(stmt)
        print('  {}:'.format(src_slice(stmt)))

        # Sort matches before printing them so that the output is guaranteed to
        # be stable.
        entities = identifier.p_entities
        for e in sorted(entities, key=lambda n: n.sloc_range.start.line):
            assert isinstance(e, lal.SubprogramSpec)
            print('    {}'.format(subp_qualname(e)))
        if not entities:
            print('    <none>')
