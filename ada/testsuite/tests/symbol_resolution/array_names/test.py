# Test the behavior of the p_entities property on various names for CallExpr
# nodes. This test in particular checks the filtering of object declarations
# that are arrays depending on the arguments provided to the CallExpr.

import libadalang as lal

import source


with open('foo.adb') as f:
    src_lines = f.readlines()
src_slice = lambda node: source.src_slice(src_lines, node.sloc_range)


ctx = lal.AnalysisContext()
unit = ctx.get_from_file('foo.adb')
unit.populate_lexical_env()


def strip_to_id(stmt):
    """
    Strip all layers in "stmt" to get the Identifier for which we want to
    resolve entities.
    """
    while not isinstance(stmt, lal.Identifier):
        if isinstance(stmt, lal.CallExpr):
            stmt = stmt.f_name
        elif isinstance(stmt, lal.Prefix):
            stmt = stmt.f_suffix
        else:
            assert False, 'Unexpected node: {}'.format(stmt)
    return stmt


# Each statement is a call expression whose name is used for the symbol
# resolution test.
stmts = unit.root.find(lal.HandledStatements)
for call in stmts.f_statements:
    print('{} resolves to:'.format(src_slice(call)))

    identifier = strip_to_id(call)
    entities = identifier.p_entities

    # Sort matches before printing them so that the output is guaranteed to
    # be stable.
    for e in sorted(entities, key=lambda n: n.sloc_range.start.line):
        print('    {}'.format(src_slice(e)))
    if not entities:
        print('    <none>')
