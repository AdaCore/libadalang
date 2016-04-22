# Test the behavior of the p_entities property on various expressions that
# designates record fields for derived record types.

import libadalang as lal

import source


with open('foo.ads') as f:
    src_lines = f.readlines()
src_slice = lambda node: source.src_slice(src_lines, node.sloc_range)


ctx = lal.AnalysisContext()
unit = ctx.get_from_file('foo.ads')
unit.populate_lexical_env()


# All pragmas contain call/prefix expressions used for the symbol resolution
# test.
last_line = None
for p in unit.root.finditer(lal.PragmaNode):
    if last_line is not None and p.sloc_range.start.line - last_line > 1:
        print ''
    last_line = p.sloc_range.start.line

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
