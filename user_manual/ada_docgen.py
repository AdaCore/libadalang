#! /usr/bin/env python

from __future__ import absolute_import, division, print_function

import argparse
from collections import defaultdict

import libadalang as lal

from langkit.utils import dispatch_on_type

from docutils import nodes
from docutils.statemachine import ViewList

from funcy import memoize

from sphinx import addnodes as N
from sphinx.util.compat import Directive
from sphinx.util.nodes import nested_parse_with_titles


try:
    from typing import List, Dict, Tuple
    assert List and Dict and Tuple  # Silence pyflakes
except ImportError:
    pass


parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('-P', '--project', default='')
parser.add_argument('files', help='The files to analyze', nargs='+')
parser.add_argument(
    '-X', action='append', help="Scenario variables to pass along to GPR"
)


@memoize
def get_documentation(decl):
    # type: (lal.BasicDecl) -> Tuple[List[str], Dict[unicode, unicode]]
    """
    Return the documentation for given basic declaration.
    """
    annotations = {}
    doc = []

    backwards = False

    class T:
        token = None

    def next_token():
        T.token = T.token.previous if backwards else T.token.next

    if isinstance(decl, lal.BasePackageDecl):
        T.token = decl.token_start.previous
        if T.token.kind == 'Whitespace':
            T.token = T.token.previous
        backwards = True
    else:
        T.token = decl.token_end.next
        if T.token.kind == 'Whitespace' and T.token.text.count("\n") == 1:
            T.token = T.token.next

    if T.token.kind != "Comment":
        return doc, annotations

    indent_level = -1
    while T.token and T.token.kind in ["Comment", "Whitespace"]:
        if T.token.kind == 'Whitespace':
            if T.token.text.count("\n") > 1:
                break

        if T.token.kind == "Comment":
            t = T.token.text[2:]  # Strip the "--"
            if t.startswith("%"):
                key, val = map(unicode.strip, t[1:].split(":"))
                annotations[key] = val
                T.token = T.token.next
                continue

            # Compute the indentation level for the whole comment block, if
            # processing the first comment.
            if indent_level == -1:
                indent_level = next(i for i, j in enumerate(t) if j.strip())

            # Add the text to the list of lines
            doc.append(t[indent_level:])

        next_token()

    if backwards:
        doc = list(reversed(doc))

    return doc, annotations


def get_signature(decl):
    # type: (lal.BasicDecl) -> unicode
    if decl.is_a(lal.BasicSubpDecl):
        return decl.p_subp_spec_or_null().text
    return ""


class AutoPackage(Directive):

    required_arguments = 1
    optional_arguments = 0

    option_spec = {
        'project_file': lambda x: x,
        'scenario_variables': lambda x: [s.strip() for s in x.split(',')],
    }

    def run(self):
        file_name = self.arguments[0].strip()
        raw_vars = self.options.get('scenario_variables') or []
        scen_vars = {}
        if raw_vars:
            for var in raw_vars:
                k, v = var.split("=")
                scen_vars[k] = v

        return self.emit_doc(
            self.options.get('project_file'), scen_vars, file_name
        )

    def handle_ada_decl(self, decl):
        # type: (lal.BasicDecl) -> Tuple[List[nodes.Node], N.desc_content]

        # Get the doc
        doc, _ = get_documentation(decl)

        # Create sphinx nodes
        self.indexnode = N.index(entries=[])
        node = N.desc()
        node.document = self.state.document
        signode = N.desc_signature(get_signature(decl), '')
        signode['first'] = False
        node.append(signode)

        # Do decl-type specific stuff in specialized methods
        if isinstance(decl, lal.BasicSubpDecl):
            self.handle_subprogram_decl(decl, node, signode)
        elif isinstance(decl, lal.BaseTypeDecl):
            self.handle_type_decl(decl, node, signode)
        else:
            self.handle_decl_generic(decl, node, signode)

        # Create the documentation's content
        content_node = N.desc_content()
        node.append(content_node)

        rst = ViewList()
        for i, l in enumerate(doc, 1):
            rst.append(l, "no_file.rst", i)

        nested_parse_with_titles(self.state, rst, content_node)
        return [self.indexnode, node], content_node

    @staticmethod
    def handle_subprogram_decl(decl, node, signode):
        # type: (lal.BasicSubpDecl, N.desc, N.desc_signature) -> None

        subp_spec = decl.p_subp_spec_or_null().cast(lal.SubpSpec)
        kind = "function" if subp_spec.p_returns else "procedure"
        name = decl.p_defining_name.text

        node['objtype'] = node['desctype'] = kind
        signode += N.desc_annotation(kind + ' ', kind + ' ')
        signode += N.desc_name(name, name)

        params = subp_spec.p_params

        if params:
            signode += nodes.Text(' ')
            param_list = N.desc_parameterlist()
            param_list.child_text_separator = "; "
            signode += param_list
            for param in params:
                assert isinstance(param, lal.ParamSpec)
                name = param.text
                p = N.desc_parameter(name, name)
                p.child_text_separator = "; "
                param_list += p

    @staticmethod
    def handle_type_decl(decl, node, signode):
        # type: (lal.BaseTypeDecl, N.desc, N.desc_signature) -> None
        node['objtype'] = node['desctype'] = "type"
        name = decl.p_defining_name.text

        signode += N.desc_annotation("type ", "type ")
        signode += N.desc_name(name, name)

    @staticmethod
    def handle_decl_generic(decl, node, signode):
        # type: (lal.BasicDecl, N.desc, N.desc_signature) -> None

        decl_to_names = [
            (lal.ObjectDecl,
             lambda od: "constant"
                        if od.f_has_constant.p_as_bool else "object"),
            (lal.AdaNode, lambda n: type(n).__name__)
        ]

        node['objtype'] = node['desctype'] = decl_name = dispatch_on_type(
            decl,
            decl_to_names
        )

        signode += N.desc_annotation(decl_name + " ", decl_name + " ")
        signode += N.desc_name(decl.text, decl.text)

    def emit_doc(self, project, scenario_vars, file_name):
        from tempfile import mkdtemp

        if not project:
            with open("{}/default.gpr".format(mkdtemp()), 'w') as f:
                f.write("project Default is end Default;")
                project = f.name

        ctx = lal.AnalysisContext(
            'utf-8', with_trivia=True,
            unit_provider=lal.UnitProvider.for_project(project, scenario_vars)
        )

        toplevel_decls = []
        toplevel_decls_set = set()

        def append_decl(decl):
            if decl not in toplevel_decls_set:
                toplevel_decls.append(decl)
                toplevel_decls_set.add(decl)

        associated_decls = defaultdict(list)

        u = ctx.get_from_file(file_name)
        try:
            package_decl = (
                u.root.cast(lal.CompilationUnit).f_body
                .cast(lal.LibraryItem).f_item
                .cast(lal.BasePackageDecl)
            )
        except AssertionError:
            print("Not a package")
            return

        decls = package_decl.f_public_part.f_decls
        types = {}

        for decl in list(decls):
            _, annotations = get_documentation(decl)

            if annotations.get('no-document'):
                continue

            if decl.is_a(lal.BasicSubpDecl):

                if annotations.get('belongs-to'):
                    associated_decls[
                        types[annotations['belongs-to']]].append(decl)
                else:
                    prim_type = decl.f_subp_spec.p_primitive_subp_of
                    if prim_type:
                        append_decl(prim_type)
                        associated_decls[prim_type].append(decl)

            elif decl.is_a(lal.BaseTypeDecl):
                types[decl.p_defining_name.text] = decl
                append_decl(decl)

            elif decl.is_a(lal.ObjectDecl):
                # Try to associate object decls to their type, if there is one
                # in the package.
                type_name = (decl.f_type_expr.p_designated_type_decl
                             .p_defining_name)
                t = types.get(type_name.text) if type_name else None
                if t:
                    associated_decls[t].append(decl)
                else:
                    append_decl(decl)

            elif decl.is_a(lal.BasicDecl):
                append_decl(decl)

        ret = []

        pkg_doc, annotations = get_documentation(package_decl)

        # Create the documentation's content
        wrapper_node = nodes.Element()

        rst = ViewList()
        title = package_decl.p_defining_name.text + " documentation"
        rst.append(title, "no_file.rst", 1)
        rst.append("-" * len(title), "no_file.rst", 2)

        for i, l in enumerate(pkg_doc, 3):
            rst.append(l, "no_file.rst", i)

        nested_parse_with_titles(self.state, rst, wrapper_node)

        ret += wrapper_node.children

        for decl in toplevel_decls:
            n, content_node = self.handle_ada_decl(decl)
            ret += n
            for assoc_decls in associated_decls[decl]:
                assoc_nodes, _ = self.handle_ada_decl(assoc_decls)
                content_node += assoc_nodes

        return ret
