#! /usr/bin/env python

from __future__ import absolute_import, division, print_function

from collections import defaultdict

from docutils import nodes
from docutils.parsers.rst import Directive
from docutils.statemachine import ViewList
from funcy import memoize
import libadalang as lal
from sphinx import addnodes as N
from sphinx.util.nodes import nested_parse_with_titles


try:
    from typing import Dict, List, Tuple
    assert List and Dict and Tuple  # Silence pyflakes
except ImportError:
    pass


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

    * ``no-document`` (bool): whether to disable documentation for the current
      entity. False by default.

    * ``belongs-to``: name of the type that must contain the documentation for
      the current entity.

    * ``document-value`` (bool): whether to document the value corresponding to
      object declarations. True by default.
    """

    required_arguments = 1
    optional_arguments = 0

    option_spec = {
        'project_file': lambda x: x,
        'scenario_variables': lambda x: [s.strip() for s in x.split(',')],
    }

    annotations = {
        'no-document': bool,
        'belongs-to': unicode,
        'document-value': bool,
    }

    def parse_into(self, doc, content_node):
        rst = ViewList()
        for i, l in enumerate(doc, 1):
            rst.append(l, 'no_file.rst', i)
        nested_parse_with_titles(self.state, rst, content_node)

    def decode_annotation(self, key, value):
        try:
            atype = self.annotations[key]
        except KeyError:
            self.warn('Unknown annotation: {}', key)

        if atype is bool:
            return {'True': True, 'False': False}[value]
        elif atype is unicode:
            return value
        else:
            assert False

    def warn(self, message, *args, **kwargs):
        self.state.document.reporter.warning(message.format(*args, **kwargs))

    @memoize
    def get_documentation(self, decl):
        # type: (lal.BasicDecl) -> Tuple[List[str], Dict[unicode, unicode]]
        """
        Return the documentation for given basic declaration.

        The returned tuple contains:

        1. the list of lines that constitutes the documentation for ``decl``;
        2. a mapping (key: string, value: string) for the parsed annotations.
        """
        try:
            doc = decl.p_doc.splitlines()
            annots = {a.key: self.decode_annotation(a.key, a.value)
                      for a in decl.p_doc_annotations}
        except lal.PropertyError:
            self.warn('Badly formatted doc for {}'.format(decl.entity_repr))
            return [], {}

        return doc, annots

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

    def handle_signature_decl(self, decl):
        # type: (lal.BasicDecl) -> Tuple[List[nodes.Node], N.desc_content]

        # Get the documentation content
        doc, annotations = self.get_documentation(decl)

        # Create sphinx nodes
        self.indexnode = N.index(entries=[])
        node = N.desc()
        node.document = self.state.document
        signode = N.desc_signature('', '')
        signode['first'] = False
        node.append(signode)

        # Do decl-type specific stuff in specialized methods
        for h in [
            ((lal.BasicSubpDecl, lal.ExprFunction),
             self.handle_subprogram_decl),
            (lal.BaseTypeDecl, self.handle_type_decl),
            (lal.ObjectDecl, self.handle_object_decl),
            (lal.PackageRenamingDecl, self.handle_package_renaming_decl),
            (lal.GenericPackageInstantiation, self.handle_package_inst),
            (lal.GenericSubpInstantiation, self.handle_subp_inst),
            (lal.ExceptionDecl, self.handle_exception_decl),
        ]:
            types, handler = h[:-1], h[-1]
            if isinstance(decl, types):
                handler(decl, node, signode, annotations)
                break
        else:
            self.handle_decl_generic(decl, node, signode, annotations)

        # Create the documentation's content
        content_node = N.desc_content()
        node.append(content_node)
        self.parse_into(doc, content_node)
        return [self.indexnode, node], content_node

    def handle_subprogram_decl(self, decl, node, signode, annotations):
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

        if isinstance(decl, lal.AbstractSubpDecl):
            signode += N.desc_annotation(' is abstract', ' is abstract')

    def handle_type_decl(self, decl, node, signode, annotations):
        # type: (lal.BaseTypeDecl, N.desc, N.desc_signature) -> None
        node['objtype'] = node['desctype'] = 'type'
        name = decl.p_defining_name.text

        signode += N.desc_annotation('type ', 'type ')
        signode += N.desc_name(name, name)

    def handle_object_decl(self, decl, node, signode, annotations):
        # type: (lal.ObjectTypeDecl, N.desc, N.desc_signature) -> None
        node['objtype'] = node['desctype'] = 'object'
        name = decl.p_defining_name.text

        if decl.f_default_expr and annotations.get('document-value', True):
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

    def handle_package_renaming_decl(self, decl, node, signode, annotations):
        # type: (lal.PackageRenamingDecl, N.desc, N.desc_signature) -> None
        node['objtype'] = node['desctype'] = decl.kind_name

        name = decl.p_defining_name.text
        renamed = decl.f_renames.f_renamed_object.text

        signode += N.desc_annotation('package ', 'package ')
        signode += N.desc_name(name, name)
        signode += N.desc_annotation(' renames ', ' renames ')
        signode += N.desc_addname(renamed, renamed)

    def handle_package_inst(self, decl, node, signode, annotations):
        # type: (lal.PackageRenamingDecl, N.desc, N.desc_signature) -> None
        node['objtype'] = node['desctype'] = decl.kind_name

        name = decl.p_defining_name.text
        last_token = (decl.f_params.token_end.next
                      if decl.f_params else decl.generic_pkg_name.token_end)
        rest = lal.Token.text_range(decl.p_defining_name.token_start.next,
                                    last_token)

        signode += N.desc_annotation('package ', 'package ')
        signode += N.desc_name(name, name)
        signode += N.desc_annotation(rest, rest)

    def handle_subp_inst(self, decl, node, signode, annotations):
        # type: (lal.PackageRenamingDecl, N.desc, N.desc_signature) -> None
        node['objtype'] = node['desctype'] = decl.kind_name

        kind = decl.token_start.text + u' '
        name = decl.p_defining_name.text
        last_token = (decl.f_params.token_end.next
                      if decl.f_params else decl.generic_subp_name.token_end)
        rest = lal.Token.text_range(decl.p_defining_name.token_start.next,
                                    last_token)

        signode += N.desc_annotation(kind, kind)
        signode += N.desc_name(name, name)
        signode += N.desc_annotation(rest, rest)

    def handle_exception_decl(self, decl, node, signode, annotations):
        # type: (lal.ExceptionDecl, N.desc, N.desc_signature) -> None
        node['objtype'] = node['desctype'] = decl.kind_name

        name = decl.p_defining_name.text

        signode += N.desc_name(name, name)
        signode += N.desc_annotation(' : exception', ' : exception')

    def handle_decl_generic(self, decl, node, signode, annotations):
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

        self.unit = ctx.get_from_file(file_name)

        if self.unit.diagnostics:
            self.error('Parsing error in {}'.format(file_name))
            for diag in self.unit.diagnostics:
                self.error(
                    "{}:{}".format(str(diag.sloc_range.start), diag.message)
                )

        if not self.unit.root:
            self.error('{} is empty'.format(file_name))

        try:
            package_decl = (
                self.unit.root.cast(lal.CompilationUnit).f_body
                .cast(lal.LibraryItem).f_item
                .cast(lal.BasePackageDecl)
            )
        except AssertionError:
            print('Not a package')
            return

        content = []
        self.handle_package(package_decl, content)
        return content

    def handle_package(self, package_decl, content):
        # type: (lal.BasicDecl, List[nodes.Node])

        # Each declaration can group the documentation of several other
        # declarations. This mapping (decl -> list[decl]) describes this
        # grouping.
        associated_decls = defaultdict(list)

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

        # Go through all declarations that appear in the top-level package and
        # organize them in sections the way we want to document them.

        decls = [d for d in package_decl.f_public_part.f_decls
                 if d.is_a(lal.BasicDecl)]

        types = {}

        for decl in decls:
            _, annotations = self.get_documentation(decl)

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
                    prim_type = decl.f_subp_spec.p_first_primitive_subp_of
                    if prim_type and prim_type.unit == self.unit:
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
                if not decl.is_a(lal.ExceptionDecl,
                                 lal.PackageRenamingDecl,
                                 lal.GenericPackageInstantiation,
                                 lal.GenericSubpInstantiation):
                    self.warn('default entity handling for {}:{}',
                              decl.unit.filename, decl)
                append_decl(decl)

        # Get documentation for the top-level package itself
        pkg_doc, annotations = self.get_documentation(package_decl)

        # Create the documentation's content

        # Create a section
        pn = package_decl.p_defining_name.text
        normalize_pn = pn.replace(".", "-").replace("_", "-").lower()
        section = nodes.section(ids=normalize_pn)
        section['names'].append(normalize_pn)

        # we create a title and we add it to section
        section += nodes.title(text=package_decl.p_defining_name.text)

        content.append(section)

        self.parse_into(pkg_doc, section)

        # Go through all entities to generate their documentation
        for decl in toplevel_decls:
            if decl.is_a(lal.PackageDecl):
                self.handle_package(decl, section)
            elif decl.is_a(lal.GenericPackageDecl):
                self.handle_package(decl.f_package_decl, section)
            else:
                n, content_node = self.handle_signature_decl(decl)
                section += n
                for assoc_decls in associated_decls[decl]:
                    assoc_nodes, _ = self.handle_signature_decl(assoc_decls)
                    content_node += assoc_nodes
