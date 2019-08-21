from __future__ import absolute_import, division, print_function

import argparse
import os.path
import re
import sys

import libadalang as lal


def unicode_image(ustr):
    result = ''
    for c in ustr:
        o = ord(c)
        if 0x20 <= o <= 0x7f:
            pass
        elif o < 0xff:
            c = '\\x{:02x}'.format(o)
        elif o < 0xffff:
            c = '\\u{:04x}'.format(o)
        else:
            c = '\\u{:08x}'.format(o)
        result += c
    return result


def escape_ascii(string):
    if isinstance(string, unicode):
        return unicode_image(string)
    else:
        return string


def entity_repr(e):
    return e.entity_repr if e else 'None'


def print_title(char, title):
    print(title)
    print(char * len(title))
    print('')


def decode_boolean_literal(node):
    assert isinstance(node, lal.Identifier)
    if node.text == 'True':
        return True
    elif node.text == 'False':
        return False
    else:
        raise ValueError('Invalid boolean literal: {}'.format(
            escape_ascii(node.text)
        ))


def resolve_node(node, show_slocs=True):
    def print_nodes(n):
        if n.is_a(lal.Expr) and (not n.is_a(lal.Name) or not n.p_is_defining):
            print('Expr: {}'.format(n))

            if n.is_a(lal.Name):
                decl_name = n.p_referenced_defining_name(
                    args.imprecise_fallback
                )

                refd_decl_img = (
                    entity_repr(decl_name)
                    if show_slocs
                    else decl_name.p_basic_decl.p_unique_identifying_name
                )
                print('  references: {}'.format(refd_decl_img))

            decl = n.p_expression_type
            decl_image = (entity_repr(decl)
                          if show_slocs or not decl else
                          decl.p_unique_identifying_name)
            print('  type:       {}'.format(decl_image))

        if ((n.p_xref_entry_point and n != node) or
                (n.is_a(lal.DefiningName) and not n.p_xref_entry_point)):
            return
        else:
            for c in n:
                if c is not None:
                    print_nodes(c)

    assert node.p_xref_entry_point

    print_title('*', "Resolving xrefs for node {}".format(node))

    # Perform name resolution on the node, using the p_resolve_names property
    # If --imprecise-fallback, proceed even if p_resolve_names has failed.
    if node.p_resolve_names or args.imprecise_fallback:
        # If it worked, print the reference value and the type value of
        # every sub expression in the node.
        print_nodes(node)
    else:
        print("Resolution failed for node {}".format(node))

    print('')

parser = argparse.ArgumentParser()
parser.add_argument(
    'files', help='Files to analyze', type=str, nargs='+', metavar='files'
)
parser.add_argument('--charset', type=str, default="")
parser.add_argument('--discard-errors-in-populate-lexical-env', '-d',
                    action='store_true')
parser.add_argument('--project', '-P', type=str)
parser.add_argument('--auto-dir', action='append')
parser.add_argument('--imprecise-fallback', action='store_true')
args = parser.parse_args()

input_sources = args.files
charset = args.charset


if args.project:
    provider = lal.UnitProvider.for_project(args.project)
elif args.auto_dir:
    input_files = []
    filename_re = re.compile(r'.*\.(ad.|a|spc|bdy)')
    for d in args.auto_dir:
        for dirpath, dirnames, filenames in os.walk(d):
            for f in filenames:
                if filename_re.match(f):
                    input_files.append(os.path.join(dirpath, f))
    provider = lal.UnitProvider.auto(input_files)
else:
    provider = None


ctx = lal.AnalysisContext(charset, unit_provider=provider)
ctx.discard_errors_in_populate_lexical_env(
    args.discard_errors_in_populate_lexical_env
)
for src_file in input_sources:
    print_title('#', 'Analyzing {}'.format(src_file))

    # Configuration for this file
    display_slocs = False
    display_short_images = False

    # Now analyze the source file
    unit = ctx.get_from_file(src_file)
    if unit.diagnostics:
        for d in unit.diagnostics:
            print('error: {}:{}'.format(src_file, d))
        sys.exit(0)
    unit.populate_lexical_env()

    empty = True

    # Print what entities are found for expressions X in all the "pragma Test
    # (X)" we can find in this unit.
    for p in unit.root.finditer(lal.PragmaNode):
        pragma_name = p.f_id.text

        if pragma_name == u'Config':
            # Handle testcase configuration pragmas for this file
            for arg in p.f_args:
                assert isinstance(arg.f_id, lal.Identifier)
                assert isinstance(arg.f_expr, lal.Identifier)
                name = arg.f_id.text
                value = arg.f_expr
                if name == u'Display_Slocs':
                    display_slocs = decode_boolean_literal(value)
                elif name == u'Display_Short_Images':
                    display_short_images = decode_boolean_literal(value)
                else:
                    raise ValueError('Invalid configuration: {}'.format(
                        escape_ascii(name)
                    ))

        elif pragma_name == u'Section':
            # Print headlines
            assert len(p.f_args) == 1
            arg = p.f_args[0].f_expr
            assert isinstance(arg, lal.StringLiteral)
            print_title('-', escape_ascii(arg.text[1:-1]))
            empty = False

        elif pragma_name == u'Test':
            # Perform name resolution
            assert len(p.f_args) == 1

            expr = p.f_args[0].f_expr
            print('{} resolves to:'.format(escape_ascii(expr.text)))

            entities = expr.p_matching_nodes

            # Sort matches before printing them so that the output is
            # guaranteed to be stable.
            for e in sorted(entities, key=lambda n: n.sloc_range.start.line):
                print('    {}{}'.format(
                    repr(e) if display_short_images else escape_ascii(e.text),
                    ' at {}'.format(e.sloc_range.start)
                    if display_slocs else ''
                ))
            if not entities:
                print('    <none>')
            empty = False

        elif pragma_name == u'Test_Statement':
            assert len(p.f_args) == 0
            resolve_node(p.previous_sibling)
            empty = False

        elif pragma_name == u'Test_Statement_UID':
            # Like Test_Statement, but will show the unique_identifying name of
            # the declarations instead of the node image.  This is used in case
            # the node might change (for example in tests where we resolve
            # runtime things).

            assert len(p.f_args) == 0
            resolve_node(p.previous_sibling, show_slocs=False)
            empty = False

        elif pragma_name == u'Test_Block':
            assert len(p.f_args) == 0
            block = (p.parent.parent.f_body
                     if p.parent.parent.is_a(lal.CompilationUnit)
                     else p.previous_sibling)

            for statement in block.findall(lambda n: n.p_xref_entry_point):
                resolve_node(statement)
            empty = False

    if not empty:
        print('')


print('Done.')
