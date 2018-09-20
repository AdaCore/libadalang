#! /usr/bin/env python

from __future__ import absolute_import, division, print_function

from collections import defaultdict

from docutils import nodes
from docutils.statemachine import ViewList
from funcy import memoize
import libadalang as lal
from sphinx import addnodes as N
from sphinx.util.compat import Directive
from sphinx.util.nodes import nested_parse_with_titles

try:
    from typing import List, Dict, Tuple
    assert List and Dict and Tuple  # Silence pyflakes
except ImportError:
    pass


@memoize
def get_documentation(decl):
    # type: (lal.BasicDecl) -> Tuple[List[str], Dict[unicode, unicode]]
    """
    Return the documentation for given basic declaration.

    The returned tuple contains:

    1. the list of lines that constitutes the documentation for ``decl``;
    2. a mapping (key: string, value: string) for the parsed annotations.
    """
    annotations = {}
    doc = []

    # Direction to follow when looking for comments (see below)
    backwards = False

    # Namespace so that local functions can modify ``token``
    class T:
        token = None

    def next_token():
        T.token = T.token.previous if backwards else T.token.next

    if isinstance(decl, lal.BasePackageDecl):
        # Documentation for packages is assumed to appear before the "package"
        # keyword.
        T.token = decl.token_start.previous
        if T.token.kind == 'Whitespace':
            T.token = T.token.previous
        backwards = True

    else:
        # Documentation for all other entities is assumed to appear after the
        # root node representing the entity.
        T.token = decl.token_end.next
        if T.token.kind == 'Whitespace' and T.token.text.count('\n') == 1:
            T.token = T.token.next

    # No comment in the direction expected? There is no documentation
    if T.token.kind != 'Comment':
        return doc, annotations

    # Process as many comments as possible from our starting point, until we
    # find an empty line or anything else than a comment or a whitespace.
    indent_level = None
    while T.token and T.token.kind in ['Comment', 'Whitespace']:
        if T.token.kind == 'Whitespace':
            if T.token.text.count('\n') > 1:
                break

        if T.token.kind == 'Comment':
            t = T.token.text[2:]  # Strip the '--'
            if t.startswith('%'):
                try:
                    key, val = map(unicode.strip, t[1:].split(':'))
                except ValueError:
                    assert False, 'Invalid syntax for annotation: {}'.format(t)
                annotations[key] = val
                next_token()
                continue

            # Compute the indentation level for the whole comment block, if
            # processing the first comment.
            if indent_level is None:
                indent_level = next(i for i, j in enumerate(t) if j.strip())

            # Add the text to the list of lines
            assert not t[:indent_level].strip(), 'Inconsistent indentation'
            doc.append(t[indent_level:])

        next_token()

    if backwards:
        doc = list(reversed(doc))

    return doc, annotations


class AutoPackage(Directive):
    """
    Sphinx directive to generate the documentation of an Ada package from the
    comments in the package specification source.

    This directive takes one argument: the path to the source file to parse.
    Two options are accepted:

    * ``project_file``, the path to a project file to load.

    * ``scenario_variables``, a list of scenario variables to pass to the
      project file. For instance: ``VAR1=somevalue,VAR2=othervalue``.

    Annotations can be added to Ada comments in order to control the
    documentation generation. The general syntax for annotations is::

        --% key: value

    Here are the supported annotations:

    * ``no-document`` (bool): when true, disable documentation for the current
      entity.

    * ``belongs-to``: name of the type that must contain the documentation for
      the current entity.
    """

    required_arguments = 1
    optional_arguments = 0

    option_spec = {
        'project_file': lambda x: x,
        'scenario_variables': lambda x: [s.strip() for s in x.split(',')],
    }

    def warn(self, message, *args, **kwargs):
        self.state.document.reporter.warning(message.format(*args, **kwargs))

    def run(self):
        file_name = self.arguments[0].strip()
        raw_vars = self.options.get('scenario_variables') or []
        scen_vars = {}
        if raw_vars:
            for var in raw_vars:
                k, v = var.split('=')
                scen_vars[k] = v

        return self.emit_doc(
            self.options.get('project_file'), scen_vars, file_name
        )

    def handle_ada_decl(self, decl):
        # type: (lal.BasicDecl) -> Tuple[List[nodes.Node], N.desc_content]

        # Get the documentation content
        doc, _ = get_documentation(decl)

        # Create sphinx nodes
        self.indexnode = N.index(entries=[])
        node = N.desc()
        node.document = self.state.document
        signode = N.desc_signature('', '')
        signode['first'] = False
        node.append(signode)

        # Do decl-type specific stuff in specialized methods
        if isinstance(decl, (lal.BasicSubpDecl, lal.ExprFunction)):
            self.handle_subprogram_decl(decl, node, signode)
        elif isinstance(decl, lal.BaseTypeDecl):
            self.handle_type_decl(decl, node, signode)
        elif isinstance(decl, lal.ObjectDecl):
            self.handle_object_decl(decl, node, signode)
        elif isinstance(decl, lal.PackageRenamingDecl):
            self.handle_package_renaming_decl(decl, node, signode)
        else:
            self.handle_decl_generic(decl, node, signode)

        # Create the documentation's content
        content_node = N.desc_content()
        node.append(content_node)

        rst = ViewList()
        for i, l in enumerate(doc, 1):
            rst.append(l, 'no_file.rst', i)

        nested_parse_with_titles(self.state, rst, content_node)
        return [self.indexnode, node], content_node

    @staticmethod
    def handle_subprogram_decl(decl, node, signode):
        # type: (lal.BasicSubpDecl, N.desc, N.desc_signature) -> None

        subp_spec = decl.p_subp_spec_or_null().cast(lal.SubpSpec)
        ret_type = subp_spec.p_returns
        kind = 'function' if ret_type else 'procedure'
        name = decl.p_defining_name.text

        node['objtype'] = node['desctype'] = kind
        signode += N.desc_annotation(kind + ' ', kind + ' ')
        signode += N.desc_name(name, name)

        params = subp_spec.p_params

        if params:
            signode += nodes.Text(' ')
            param_list = N.desc_parameterlist()
            param_list.child_text_separator = ''
            signode += param_list
            for i, param in enumerate(params):
                assert isinstance(param, lal.ParamSpec)
                if i > 0:
                    param_list += nodes.Text('; ')
                name = param.f_ids.text
                ptype = param.f_type_expr.text
                param_list += N.desc_parameter(name, name)
                param_list += nodes.Text(' : ')
                param_list += N.desc_type(ptype, ptype)

        if ret_type:
            signode += N.desc_annotation(' return ', ' return ')
            signode += N.desc_type(ret_type.text, ret_type.text)

    @staticmethod
    def handle_type_decl(decl, node, signode):
        # type: (lal.BaseTypeDecl, N.desc, N.desc_signature) -> None
        node['objtype'] = node['desctype'] = 'type'
        name = decl.p_defining_name.text

        signode += N.desc_annotation('type ', 'type ')
        signode += N.desc_name(name, name)

    @staticmethod
    def handle_object_decl(decl, node, signode):
        # type: (lal.ObjectTypeDecl, N.desc, N.desc_signature) -> None
        node['objtype'] = node['desctype'] = 'object'
        name = decl.p_defining_name.text

        if decl.f_default_expr:
            # If there is a default expression to describe, do it as an
            # additional description. The title will only contain the name up
            # to the type expression.
            default_expr = decl.f_default_expr.text
            content = N.desc_content()
            node.append(content)
            content += nodes.Text('Value: ')
            content += N.desc_annotation(default_expr, default_expr)

            last_token = decl.f_type_expr.token_end

        elif decl.f_renaming_clause:
            # If there is a renaming clause, just put everything until the
            # renaming clause in the title.
            last_token = decl.f_renaming_clause.token_end

        else:
            # By default, go until the type expression
            last_token = decl.f_type_expr.token_end

        descr = lal.Token.text_range(decl.p_defining_name.token_end.next,
                                     last_token)

        signode += N.desc_name(name, name)
        signode += N.desc_annotation(descr, descr)

    @staticmethod
    def handle_package_renaming_decl(decl, node, signode):
        # type: (lal.PackageRenamingDecl, N.desc, N.desc_signature) -> None
        node['objtype'] = node['desctype'] = decl.kind_name

        name = decl.p_defining_name.text
        renamed = decl.f_renames.f_renamed_object.text

        signode += N.desc_annotation('package ', 'package ')
        signode += N.desc_name(name, name)
        signode += N.desc_annotation(' renames ', ' renames ')
        signode += N.desc_addname(renamed, renamed)

    @staticmethod
    def handle_decl_generic(decl, node, signode):
        # type: (lal.BasicDecl, N.desc, N.desc_signature) -> None
        node['objtype'] = node['desctype'] = decl.kind_name
        signode += N.desc_name(decl.text, decl.text)

    def emit_doc(self, project, scenario_vars, file_name):
        """
        Entry point for the doc generation of one Ada package.

        :param None|str project: Optional path to the project file to load.

        :param scenario_vars: Mapping for scenario variables to use to load the
            project file.
        :type scenario_vars: None|dict[str,str]

        :param str file_name: Path of the Ada source file to parse.
        """

        from tempfile import mkdtemp

        if not project:
            with open('{}/default.gpr'.format(mkdtemp()), 'w') as f:
                f.write('project Default is end Default;')
                project = f.name

        ctx = lal.AnalysisContext(
            'utf-8', with_trivia=True,
            unit_provider=lal.UnitProvider.for_project(project, scenario_vars)
        )

        # List of top-level declarations to document
        toplevel_decls = []

        # Set mirorring toplevel_decls, used to check whether a decl is part of
        # it already.
        toplevel_decls_set = set()

        def append_decl(decl):
            """
            Append ``decl`` to ``toplevel_decls`` if it's not there yet.
            """
            if decl not in toplevel_decls_set:
                toplevel_decls.append(decl)
                toplevel_decls_set.add(decl)

        # Each declaration can group the documentation of several other
        # declarations. This mapping (decl -> list[decl]) describes this
        # grouping.
        associated_decls = defaultdict(list)

        u = ctx.get_from_file(file_name)
        assert not u.diagnostics, 'Parsing error in {}'.format(file_name)
        assert u.root, '{} is empty'.format(file_name)
        try:
            package_decl = (
                u.root.cast(lal.CompilationUnit).f_body
                .cast(lal.LibraryItem).f_item
                .cast(lal.BasePackageDecl)
            )
        except AssertionError:
            print('Not a package')
            return

        # Go through all declarations that appear in the top-level package and
        # organize them in sections the way we want to document them.

        decls = package_decl.f_public_part.f_decls
        types = {}

        for decl in list(decls):
            _, annotations = get_documentation(decl)

            # Skip documentation for this entity
            if annotations.get('no-document'):
                continue

            if decl.is_a(lal.BasicSubpDecl, lal.ExprFunction):
                # Look for the type under which this subprogram should be
                # documented ("owning_type"). This is either the explicitly
                # asked type ("belongs-to" annotation) or the type that is a
                # primitive for this subprogram (if the type is declared in the
                # same file).
                owning_type = None
                if annotations.get('belongs-to'):
                    owning_type = types[annotations['belongs-to']]
                else:
                    prim_type = decl.f_subp_spec.p_primitive_subp_of
                    if prim_type and prim_type.unit == u:
                        owning_type = prim_type
                        append_decl(owning_type)

                # If we found a relevant type, document the subprogram under
                # it, otherwise document it at the top-level.
                if owning_type:
                    associated_decls[owning_type].append(decl)
                else:
                    append_decl(decl)

            elif decl.is_a(lal.BaseTypeDecl):
                # New type declaration: document it and register it as a type
                types[decl.p_defining_name.text] = decl
                append_decl(decl)

            elif decl.is_a(lal.ObjectDecl):
                # Try to associate object declarations to their type, if there
                # is one in the current package.
                type_name = (decl.f_type_expr.p_designated_type_decl
                             .p_defining_name)
                t = types.get(type_name.text) if type_name else None
                if t:
                    associated_decls[t].append(decl)
                else:
                    append_decl(decl)

            elif decl.is_a(lal.BasicDecl):
                if not decl.is_a(lal.PackageRenamingDecl,
                                 lal.GenericPackageInstantiation):
                    self.warn('default entity handling for {}:{}',
                              decl.unit.filename, decl)
                append_decl(decl)

        ret = []

        # Get documentation for the top-level package itself
        pkg_doc, annotations = get_documentation(package_decl)

        # Create the documentation's content
        wrapper_node = nodes.Element()

        rst = ViewList()
        title = package_decl.p_defining_name.text
        rst.append(title, 'no_file.rst', 1)
        rst.append('-' * len(title), 'no_file.rst', 2)

        for i, l in enumerate(pkg_doc, 3):
            rst.append(l, 'no_file.rst', i)

        nested_parse_with_titles(self.state, rst, wrapper_node)

        ret += wrapper_node.children

        # Go through all entities to generate their documentation
        for decl in toplevel_decls:
            n, content_node = self.handle_ada_decl(decl)
            ret += n
            for assoc_decls in associated_decls[decl]:
                assoc_nodes, _ = self.handle_ada_decl(assoc_decls)
                content_node += assoc_nodes

        return ret
