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


# All pragmas contain call/prefix expressions used for the symbol resolution
# test.
for p in unit.root.finditer(lal.PragmaNode):
    assert len(p.f_args) == 1
    expr = p.f_args[0].f_expr
    print('{} resolves to:'.format(src_slice(expr)))

    entities = expr.p_entities

    # Sort matches before printing them so that the output is guaranteed to
    # be stable.
    for e in sorted(entities, key=lambda n: n.sloc_range.start.line):
        print('    {}'.format(src_slice(e)))
    if not entities:
        print('    <none>')
