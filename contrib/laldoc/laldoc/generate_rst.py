#! /usr/bin/env python
"""
This module forms the basis of the "new" laldoc. It is completely
distinct from the ``AutoPackage`` directive found in __init__.py.

It depends on the reworked Ada domain that can be found at
<https://github.com/AdaCore/sphinxcontrib-adadomain>.

This module is a command line program that will generate RST files from an Ada
project, given a list of API files to take into account.

We might want to add a sphinx directive like in the old laldoc at some stage,
but having it separate demonstrates how we could integrate such a mechanism
into GNATDoc for example, in pure Ada.

The workflow for the moment is hence to:

1. Generate the RST files as part of the doc build
2. Run sphinx make on the resulting doc, making sure to install
``sphinxcontrib.adadomain`` and add it to the list of extensions.
"""


from collections import defaultdict
from contextlib import contextmanager
import os
from os import path as P
import re
import sys
from typing import Dict, List, Optional as Opt, Set, Tuple, Union

import libadalang as lal


PY3 = sys.version_info[0] == 3
if PY3:
    from functools import lru_cache as memoize
else:
    from funcy import memoize


UNDERLINES = ["-", "^", "\""]


def strip_ws(strn: str) -> str:
    """
    Strip whitespace from ``strn``.
    """
    return re.sub(r"\s+", " ", strn)


class GenerateDoc(lal.App):
    """
    Main class for the documentation generator app, using the lal.App
    base class.
    """

    annotations = {
        'no-document': bool,  # Don't document associated entity

        'belongs-to': str,  # Attach associated entity to entity given in
                            # parameter

        'document-value': bool,  # Whether to document the value of an object
                                 # decl or not. Default is True
    }

    lines: List[str]
    _indent: int
    _package_nesting_level: int

    def add_string(self, strn: str):
        """
        Add ``strn`` to the rst output.
        """
        self.add_lines(strn.splitlines())

    def add_lines(self, lines: List[str]):
        """
        Add given lines to the rst output.
        """
        for line in lines:
            self.lines.append(f"{' ' * self._indent}{line}")

    def add_arguments(self):
        self.parser.add_argument(
            '-O', '--output-dir', type=str,
            default=".",
            help='Output directory for the generated rst files'
        )
        super(GenerateDoc, self).add_arguments()

    @contextmanager
    def indent(self):
        """
        Context manager to indent sphinx code emitted inside the with block.
        """
        try:
            self._indent += 4
            yield
        finally:
            self._indent -= 4

    @staticmethod
    def error(error_message: str):
        """
        Print an error message and exit.
        """
        print(error_message)
        exit(1)

    @staticmethod
    def warn(error_message: str):
        """
        Print a warning message.
        """
        print(error_message)

    def process_annotation(
        self, key: str, value: str
    ) -> Union[bool, str, None]:
        """
        Process an annotation for an entity, return its structured value.
        """
        try:
            atype = self.annotations[key]
        except KeyError:
            self.warn(f'Unknown annotation: {key}')
            return

        if atype is bool:
            return {'True': True, 'False': False}[value]
        elif atype is str:
            return value
        else:
            assert False

    @staticmethod
    def process_docstring(strn: str) -> str:
        """
        Docstring preprocessing, handling laldoc specific syntax:

        * Transform ``@var`` annotations into  ``:ada:ref:var`` annotations.
        """
        # Split the doc on `` literals to avoid doing substitutions of @ syntax
        # inside inline literal blocks.
        split_doc = re.split(r"(``.+?``)", strn)
        for i, chunk in enumerate(split_doc):
            if chunk.startswith('`'):
                continue

            split_doc[i] = re.sub(
                r"@([\w.]+)",
                lambda m: f":ada:ref:`{m.groups()[0]}`",
                chunk
            )
        return "".join(split_doc)

    @memoize
    def get_documentation(
        self, decl: lal.BasicDecl
    ) -> Tuple[List[str], Dict[str, str]]:
        """
        Return the documentation for given basic declaration.

        The returned tuple contains:

        1. the list of lines that constitutes the documentation for ``decl``;
        2. a mapping (key: string, value: string) for the parsed annotations.
        """
        try:
            doc = self.process_docstring(decl.p_doc).splitlines()
            annots = {a.key: self.process_annotation(a.key, a.value)
                      for a in decl.p_doc_annotations}
        except lal.PropertyError:
            self.warn('Badly formatted doc for {}'.format(decl.entity_repr))
            return [], {}

        return doc, annots

    def main(self) -> None:
        self.lines = []
        self._indent = 0
        self._package_nesting_level = 0

        os.makedirs(self.args.output_dir, exist_ok=True)

        # Sort unit by filename to have a deterministic processing order
        for _, unit in sorted(self.units.items()):
            self.process_unit(unit)

    @property
    def description(self) -> str:
        return """
        Generate ReST output from Ada files. Only files containing
        library level packages are handled
        """

    def process_unit(self, unit: lal.AnalysisUnit) -> None:
        """
        Process one LAL analysis unit.
        """
        if unit.diagnostics:
            self.error('Parsing error in {}'.format(unit.filename))
            for diag in unit.diagnostics:
                self.error(
                    "{}:{}".format(str(diag.sloc_range.start), diag.message)
                )

        if not unit.root:
            self.error('{} is empty'.format(unit.filename))

        try:
            decl = (
                unit.root.cast(lal.CompilationUnit).f_body
                .cast(lal.LibraryItem).f_item
            )

            package_decl = decl.cast(lal.BasePackageDecl)
            self.handle_package(package_decl)

            out_file = P.join(self.args.output_dir,
                              P.basename(P.splitext(unit.filename)[0]))
            with open(f"{out_file}.rst", "w") as f:
                f.write("\n".join(line.rstrip() for line in self.lines))

            self.lines = []
        except AssertionError:
            print(f"WARNING: Non handled top level decl: {decl}")
            return

    def handle_package(
        self,
        package_decl: lal.BasePackageDecl,
        gen_package: Opt[lal.GenericPackageDecl] = None
    ) -> None:
        """
        Handle a package declaration. This method is called recursively, to
        share code, and will have a different behavior when the package is a
        toplevel one or a nested one.
        """
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

        decls = [d.cast(lal.BasicDecl)
                 for d in package_decl.f_public_part.f_decls
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
                    prim_type = decl.f_subp_spec.p_primitive_subp_first_type()
                    if prim_type and prim_type.unit == package_decl.unit:
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
                # If decl is not a decl that we explicitly know should be
                # handled the default way, warn.
                if not decl.is_a(lal.ExceptionDecl,
                                 lal.PackageRenamingDecl,
                                 lal.GenericPackageInstantiation,
                                 lal.GenericSubpInstantiation,
                                 lal.GenericPackageDecl,
                                 lal.PackageDecl):
                    self.warn('default entity handling for '
                              f'{P.relpath(decl.unit.filename)}:{decl}')
                append_decl(decl)

        # Get documentation for the top-level package itself
        pkg_doc, annotations = self.get_documentation(package_decl)

        # Create the documentation's content

        # Create a section
        pkg_name = package_decl.p_fully_qualified_name
        self.add_lines([''])

        def handle_decl(decl):
            if decl.is_a(lal.PackageDecl):
                self.handle_package(decl)
            elif decl.is_a(lal.GenericPackageDecl):
                self.handle_package(decl.f_package_decl, gen_package=decl)
            else:
                self.handle_entity(decl)
                with self.indent():
                    for assoc_decls in associated_decls[decl]:
                        self.handle_entity(assoc_decls)

        if self._package_nesting_level == 0:
            self.add_string(
                f"{pkg_name }\n"
                f"{UNDERLINES[self._package_nesting_level] * len(pkg_name )}"
            )
            self.add_lines(['', f".. ada:set_package:: {pkg_name}"])
        else:
            generic = "generic_" if gen_package is not None else ""
            self.add_lines([f".. ada:{generic}package:: {pkg_name}", ""])
            self._indent += 4

        self.add_lines([''] + pkg_doc + [''])

        if gen_package is not None:
            self.add_lines([':Formals:'])

            with self.indent():
                for decl in gen_package.f_formal_part.f_decls:
                    handle_decl(decl)

        # Go through all entities to generate their documentation
        self._package_nesting_level += 1
        for decl in toplevel_decls:
            handle_decl(decl)
        self._package_nesting_level -= 1

        if self._package_nesting_level != 0:
            self._indent -= 4

    def handle_entity(self, decl: lal.BasicDecl):

        def make_profile(s: lal.BaseSubpSpec) -> str:
            """
            Reconstruct a text profile for given subprogram spec, with fully
            qualified type names.
            """

            def typ(te: lal.TypeExpr) -> str:
                # TODO: Anonymous types are not handled fully yet: we just
                # grab their text, but we should expand inner type names too to
                # be fully qualified.
                if te.is_a(lal.AnonymousType):
                    return strip_ws(te.text)
                else:
                    return te.p_designated_type_decl.p_fully_qualified_name

            params = "({})".format("; ".join(
                f"{strip_ws(p.f_ids.text)} : "
                f"{typ(p.f_type_expr)}"
                for p in s.f_subp_params.f_params
            )) if s.f_subp_params else ""

            returns = (
                f"return {typ(s.f_subp_returns)}" if s.f_subp_returns else ""
            )
            ret = (
                f"{s.f_subp_kind.text} {s.f_subp_name.text}"
                f" {params} {returns}"
            )
            return ret

        # Get the documentation content
        doc, annotations = self.get_documentation(decl)

        def emit_directive(directive_header):
            self.add_lines([
                directive_header,
            ])
            with self.indent():
                self.add_lines([
                    ":package: "
                    f"{decl.p_parent_basic_decl.p_fully_qualified_name}"
                ])

        if isinstance(decl, (lal.BasicSubpDecl, lal.ExprFunction)):
            subp_spec = decl.p_subp_spec_or_null()
            prof = make_profile(subp_spec)
            subp_kind = (
                'procedure' if subp_spec.p_returns is None
                else 'function'
            )
            emit_directive(f".. ada:{subp_kind}:: {prof}")

            for formal in decl.p_subp_spec_or_null().p_abstract_formal_params:
                formal_doc, annots = self.get_documentation(formal)

                # Only generate a param profile if you have doc to show.
                # TODO: This is weird, because params without doc will not be
                # shown. Ideally it would be better to switch on if any param
                # has doc.
                if formal_doc:
                    for i in formal.p_defining_names:
                        fqn = formal.p_formal_type().p_fully_qualified_name
                        self.add_string(f":param {fqn} {i.text}:")
                        with self.indent():
                            self.add_lines(doc)

        elif isinstance(decl, lal.BaseTypeDecl):
            if isinstance(decl, lal.IncompleteTypeDecl):
                return

            prof = f"type {decl.p_relative_name.text}"
            emit_directive(f".. ada:type:: {prof}")

            with self.indent():
                self.add_lines([''])

                # Register components (discriminants and fields)
                comps: Dict[lal.BaseFormalParamDecl,
                            Set[Tuple[lal.DiscriminantValues]]] = {}

                if decl.p_is_access_type():
                    pass
                elif decl.p_is_record_type():
                    try:
                        for shape in decl.p_shapes():
                            for comp in shape.components:
                                ctx = comp.parent.parent.parent
                                s = comps.setdefault(comp, set())
                                if not ctx.is_a(lal.Variant):
                                    s.add(tuple(shape.discriminants_values))
                    except lal.PropertyError:
                        # TODO TA20-019: p_shapes will fail on some types that
                        # are considered records, so we should not crash on
                        # this.
                        pass
                else:
                    for comp in decl.p_discriminants_list():
                        comps[comp] = set()

                # Emit components
                for comp, discrs in comps.items():
                    doc, annots = self.get_documentation(comp)
                    for dn in comp.p_defining_names:
                        formal_type = comp.p_formal_type()
                        if formal_type.is_a(lal.AnonymousTypeDecl):
                            tn = "``{}``".format(
                                formal_type.text
                            )
                        else:
                            tn = comp.p_formal_type().p_fully_qualified_name
                        comp_kind = (
                            "discriminant" if comp.is_a(lal.DiscriminantSpec)
                            else "component"
                        )
                        self.add_string(f":{comp_kind} {tn} {dn.text}:")
                        with self.indent():
                            self.add_lines(doc)

        elif isinstance(decl, lal.ObjectDecl):
            default_expr = None

            if decl.f_default_expr and annotations.get('document-value', True):
                # If there is a default expression to describe, do it as an
                # additional description. The title will only contain the name
                # up to the type expression.
                default_expr = decl.f_default_expr.text
                last_token = decl.f_type_expr.token_end

            elif decl.f_renaming_clause:
                # If there is a renaming clause, just put everything until the
                # renaming clause in the title.
                last_token = decl.f_renaming_clause.token_end

            else:
                # By default, go until the type expression
                last_token = decl.f_type_expr.token_end

            descr = strip_ws(lal.Token.text_range(
                decl.token_start, last_token
            ))

            emit_directive(f".. ada:object:: {descr}")

            with self.indent():
                self.add_lines([''])
                typ = decl.p_type_expression.p_designated_type_decl
                if typ.is_a(lal.AnonymousTypeDecl):
                    typ_str = f"``{decl.p_type_expression.text}``"
                else:
                    typ_str = typ.p_fully_qualified_name
                if not decl.parent.is_a(lal.GenericFormal):
                    self.add_string(f":objtype: {typ_str}")
                    if default_expr:
                        self.add_string(
                            f":defval: ``{strip_ws(default_expr)}``"
                        )
                    if decl.f_renaming_clause:
                        self.add_string(
                            ":renames: "
                            f"{decl.f_renaming_clause.f_renamed_object.text}"
                        )

        elif isinstance(decl, lal.ExceptionDecl):
            name = decl.p_defining_name.text
            emit_directive(f".. ada:exception:: {name}")

        elif isinstance(decl, lal.GenericPackageInstantiation):
            sig = strip_ws(lal.Token.text_range(
                decl.token_start, decl.f_generic_pkg_name.token_end
            ))
            emit_directive(f".. ada:generic-package-instantiation:: {sig}")
            with self.indent():
                self.add_lines([''])
                self.add_string(".. code-block:: ada")
                with self.indent():
                    self.add_lines([''] + decl.text.splitlines())
                self.add_lines([''])
                self.add_string(
                    ":instpkg: "
                    f"{decl.p_designated_generic_decl.p_fully_qualified_name}"
                )
        elif isinstance(decl, lal.GenericFormal):
            self.handle_entity(decl.f_decl)
            return
        else:
            print(f"WARNING: Non handled entity: {decl}")

        with self.indent():
            self.add_lines([''] + doc + [''])


if __name__ == '__main__':
    GenerateDoc.run()
