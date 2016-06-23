import sys

import libadalang as lal

import source


def print_title(char, title):
    print(title)
    print(char * len(title))
    print('')


def decode_boolean_literal(node):
    assert isinstance(node, lal.Identifier)
    if node.f_tok.text == 'True':
        return True
    elif node.f_tok.text == 'False':
        return False
    else:
        raise ValueError('Invalid boolean literal: {}'.format(node.f_tok.text))


# Associate a list of source lines for each source visited files. Used to
# display source excerpts.
lines_map = {}


def get_lines(src_file):
    try:
        return lines_map[src_file]
    except KeyError:
        pass

    with open(src_file) as f:
        lines_map[src_file] = f.readlines()
    return lines_map[src_file]


def src_slice(node):
    lines = get_lines(node.unit.filename)
    return source.src_slice(lines, node.sloc_range)


def resolve_statement(statement):
    assert isinstance(statement, lal.Statement)

    # Perform symbol resolution on the preceding statement, using the
    # p_resolve_symbols property.
    if statement.p_resolve_symbols:
        # If it worked, print the reference value and the type value of
        # every sub expression in the statement.
        for expr in statement.findall(lal.Expr):
            print "Expr: {}, references {}, type is {}".format(
                expr, expr.p_ref_val, expr.p_type_val
            )
    else:
        print "Resolution failed for statement {}".format(statement)


ctx = lal.AnalysisContext()
for src_file in sys.argv[1:]:
    print_title('#', 'Analyzing {}'.format(src_file))

    # Configuration for this file
    display_slocs = False

    # Now analyze the source file
    unit = ctx.get_from_file(src_file)
    if unit.diagnostics:
        for d in unit.diagnostics:
            print('error: {}:{}'.format(src_file, d))
        sys.exit(0)
    unit.populate_lexical_env()

    last_line = None

    # Print what entities are found for expressions X in all the "pragma Test
    # (X)" we can find in this unit.
    for p in unit.root.finditer(lal.PragmaNode):
        pragma_name = p.f_id.f_tok.text

        # If this pragma and the previous ones are not on adjacent lines, do
        # not make them adjacent in the output.
        if pragma_name != 'Config':
            if (last_line is not None and
                    p.sloc_range.start.line - last_line > 1):
                print ''
            last_line = p.sloc_range.start.line

        if pragma_name == 'Config':
            # Handle testcase configuration pragmas for this file
            for arg in p.f_args:
                assert isinstance(arg.f_id, lal.Identifier)
                assert isinstance(arg.f_expr, lal.Identifier)
                name = arg.f_id.f_tok.text
                value = arg.f_expr
                if name == 'Display_Slocs':
                    display_slocs = decode_boolean_literal(value)
                else:
                    raise ValueError('Invalid configuration: {}'.format(name))

        elif pragma_name == 'Section':
            # Print headlines
            assert len(p.f_args) == 1
            arg = p.f_args[0].f_expr
            assert isinstance(arg, lal.StringLiteral)
            print_title('-', arg.f_tok.text[1:-1])

        elif pragma_name == 'Test':
            # Perform symbol resolution
            assert len(p.f_args) == 1

            expr = p.f_args[0].f_expr
            print('{} resolves to:'.format(src_slice(expr)))

            entities = expr.p_entities

            # Sort matches before printing them so that the output is
            # guaranteed to be stable.
            for e in sorted(entities, key=lambda n: n.sloc_range.start.line):
                print('    {}{}'.format(
                    src_slice(e),
                    ' at {}'.format(e.sloc_range.start)
                    if display_slocs else ''
                ))
            if not entities:
                print('    <none>')

        elif pragma_name == 'Test_Statement':
            assert not p.f_args
            resolve_statement(p.previous_sibling)

        elif pragma_name == 'Test_Block':
            assert not p.f_args
            for statement in p.previous_sibling.findall(lal.SimpleStatement):
                resolve_statement(statement)


print('')
print('Done.')
