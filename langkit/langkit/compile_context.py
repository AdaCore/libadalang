"""
This file contains the logic for the compilation context for langkit. This is
the main hook into langkit, insofar as this is the gate through which an
external language creator will actually trigger the code emission. For example,
this is the way it is done for the ada language::

    from ada_parser import ada_lexer, ada_grammar
    context = CompileCtx(... ada_lexer, ada_grammar...)
    ...
    context.emit(...)
"""
from collections import defaultdict
from glob import glob
from distutils.spawn import find_executable
import itertools
import names
import os
from os import path, environ
import shutil
import sys
import subprocess

from ada_api import AdaAPISettings
from c_api import CAPISettings
import caching
from python_api import PythonAPISettings
from utils import Colors, printcol


compile_ctx = None


def get_context():
    """
    Returns the current compilation context. Meant to be used by the rest of
    LangKit, in any code that has been called as part of the CompileCtx.emit
    primitive.
    """
    assert compile_ctx is not None, (
        "Get context has been called in a state in which the compile context"
        " is not set"
    )
    return compile_ctx


def write_cpp_file(file_path, source):
    with open(file_path, "wb") as out_file:
        if find_executable("clang-format"):
            p = subprocess.Popen(["clang-format"], stdin=subprocess.PIPE,
                                 stdout=out_file)
            p.communicate(source)
            assert p.returncode == 0
        else:
            out_file.write(source)


ada_spec = "spec"
ada_body = "body"


def write_ada_file(path, source_kind, qual_name, source):
    assert source_kind in (ada_spec, ada_body)
    file_name = "{}.{}".format("-".join(qual_name).lower(),
                               "ads" if source_kind == ada_spec else "adb")
    file_path = os.path.join(path, file_name)

    # TODO: no tool is able to pretty-print a single Ada source file
    with open(file_path, "wb") as out_file:
            out_file.write(source)


class CompileCtx():
    """State holder for native code emission."""

    def __init__(self, lang_name, main_rule_name, lexer, grammar,
                 lib_name=None,
                 c_symbol_prefix=None,
                 enable_python_api=True,
                 verbose=False):
        """Create a new context for code emission.

        :param str lang_name: string (mixed case and underscore: see
        langkit.names.Name) for the Name of the target language.

        :param str main_rule_name: Name for the grammar rule that will be used
        as an entry point when parsing units.

        :param lexer: A lexer for the target language.
        :type lexer: langkit.lexer.Lexer

        :param grammar: A grammar for the target language.
        :type grammar: langkit.parsers.Grammer

        :param lib_name: If provided, must be a string (mixed case and
        underscore: see langkit.names.Name), otherwise set to
        "Lib<lang_name>lang". It is used for the filenames, package names, etc.
        in the generated library.
        :type lib_name: str or None

        :param c_symbol_prefix: Valid C identifier used as a prefix for all
        top-level declarations in the generated C API.  If not provided, set to
        the name of the language in lower case.  Empty string stands for no
        prefix.
        :type c_symbol_prefix: str or None

        :param bool enable_python_api: If True (which is the default),
        generates a Python API for the generated library.

        :param bool verbose: If True (which is not the default), print various
        debug messages on standard output.
        """
        self.lang_name = names.Name(lang_name)
        self.main_rule_name = main_rule_name

        lib_name = (
            names.Name('Lib{}lang'.format(
                self.lang_name.camel_with_underscores
            ))
            if lib_name is None else
            names.Name(lib_name)
        )

        self.ada_api_settings = AdaAPISettings(
            lib_name.camel_with_underscores
        )
        self.c_api_settings = CAPISettings(lib_name.lower,
                                           (self.lang_name.lower
                                            if c_symbol_prefix is None else
                                            c_symbol_prefix))
        self.verbose = verbose

        # Mapping: rule name -> Parser instances.
        # TODO: why do we need this? The grammar already has such a mapping.
        self.rules_to_fn_names = {}

        # Lexer instance
        self.lexer = lexer

        # Grammar instance
        self.grammar = grammar

        self.python_api_settings = (
            PythonAPISettings(lib_name.lower, self.c_api_settings)
            if enable_python_api else None
        )

        # Set of names (names.Name instances) for all generated parser
        # functions. This is used to avoid generating these multiple times.
        self.fns = set()

        # Set of CompiledType subclasses: all such subclasses must register
        # themselves here when their add_to_context method is invoked. This
        # field too is used to avoid multiple generation issues.
        self.types = set()

        # List for all ASTnode subclasses (ASTNode excluded), sorted so that A
        # is before B when A is a parent class for B. This sorting is important
        # to output declarations in dependency order.
        # This is computed right after field types inference.
        self.astnode_types = []

        # Set of all ASTNode subclasses (ASTNode included) for which we
        # generate a corresponding list type.
        self.list_types = set()

        #
        # Holders for the Ada generated code chunks
        #

        # List of TypeDeclaration instances for all enumeration types used in
        # AST node fields and all ASTNode subclasses.
        self.enum_declarations = []

        # List of TypeDeclaration instances for all ASTNode derivations
        # (excluding ASTList derivations). These contain the type full
        # declarations.
        self.types_declarations = []

        # List of TypeDeclaration instances for all ASTNode derivations. These
        # only contain forward declarations so that full declarations have
        # access to all declared types.
        self.incomplete_types_declarations = []

        # List of TypeDeclaration instances for all ASTList derivations. These
        # don't need forward declarations.
        self.list_types_declarations = []

        # List of strings for all ASTNode subclasses primitives body
        self.primitives_bodies = []

        # List of GeneratedParser instances
        self.generated_parsers = []

        #
        # Holders for the C external API generated code chunks
        #

        # Mapping: ASTNode concrete (i.e. non abstract) subclass -> int,
        # associating specific constants to be used reliably in bindings.  This
        # mapping is built at the beginning of code emission.
        self.node_kind_constants = {}

        # Mapping: ASTNode concrete (i.e. non abstract) subclass -> int,
        # associating specific constants to be used reliably in bindings.  This
        # mapping is built at the beginning of code emission.
        self.node_kind_constants = {}

        # Mapping: ASTNode subclass -> GeneratedFunction instances for all
        # subclass field accessors.
        self.c_astnode_primitives = defaultdict(list)

        # Mapping: CompiledType -> string (C declarations) for types used in
        # AST node fields.
        self.c_astnode_field_types = {}

        # Likewise but for Ada declarations
        self.c_astnode_field_types_ada = {}

        #
        # Corresponding holders for the Python API
        #

        # Mapping: ASTNode subclass -> string (generated Python code ASTNode
        # subclass declarations).
        self.py_astnode_subclasses = {}

        # Mapping CompiledType -> string (Python declarations) for types used
        # in AST node fields.
        self.py_astnode_field_types = {}

        self.cache = None

        # Internal field for extensions directory
        self._extensions_dir = None

    def set_ast_fields_types(self, astnode, types):
        """
        Associate `types` (a list of CompiledType) to fields in `astnode` (an
        ASTNode sub-class). It is valid to perform this association multiple
        times as long as types are consistent.
        """
        fields = astnode.get_fields(include_inherited=False)

        assert len(fields) == len(types), (
            "{} has {} fields ({} types given). You probably have"
            " inconsistent grammar rules and type declarations".format(
                astnode, len(fields), len(types)
            )
        )

        def is_subtype(base_type, subtype):
            return issubclass(subtype, base_type)

        def are_subtypes(fields, new_types):
            return all(
                is_subtype(f.type, n)
                for f, n in zip(fields, new_types)
            )

        # TODO: instead of expecting types to be *exactly* the same, perform
        # type unification (take the nearest common ancestor for all field
        # types).
        assert (not astnode.is_type_resolved or
                are_subtypes(fields, types)), (
            "Already associated types for some fields are not consistent with"
            " current ones:\n- {}\n- {}".format(
                [f.type for f in fields], types
            )
        )

        # Only assign types if astnode was not yet typed. In the case where it
        # was already typed, we checked above that the new types were
        # consistent with the already present ones.
        if not astnode.is_type_resolved:
            astnode.is_type_resolved = True
            self.astnode_types.append(astnode)
            for field_type, field in zip(types, fields):
                field.type = field_type

    def order_astnode_types(self):
        """Sort the "astnode_types" field."""
        # Sort them in dependency order as required but also then in
        # alphabetical order so that generated declarations are kept in a
        # relatively stable order. This is really useful for debugging
        # purposes.
        keys = {
            cls: ".".join(cls.name().base_name
                          for cls in cls.get_inheritance_chain())
            for cls in self.astnode_types
        }
        self.astnode_types.sort(key=lambda cls: keys[cls])

    def render_template(self, *args, **kwargs):
        # Kludge: to avoid circular dependency issues, do not import parsers
        # until needed.
        # TODO: If the render method was dynamically bound, like the compile
        # context, rather than being explicitly redefined in every module, we
        # could avoid this, maybe.
        from parsers import render
        return render(*args, **kwargs)

    def emit(self, file_root="."):
        global compile_ctx
        try:
            compile_ctx = self
            self._emit(file_root)
        finally:
            compile_ctx = None

    def _emit(self, file_root):
        """
        Emit native code for all the rules in this grammar as a library:
        a library specification and the corresponding implementation.  Also
        emit a tiny program that can parse starting with any parsing rule for
        testing purposes.
        """
        assert self.grammar, "Set grammar before calling emit"

        lib_name_low = self.ada_api_settings.lib_name.lower()

        include_path = path.join(file_root, "include")
        src_path = path.join(file_root, "include", lib_name_low)
        lib_path = path.join(file_root, "lib")
        share_path = path.join(file_root, "share", lib_name_low)

        if not path.exists(file_root):
            os.mkdir(file_root)

        printcol("File setup ...", Colors.OKBLUE)

        for d in ["include",
                  "include/langkit_support",
                  "include/{}".format(lib_name_low),
                  "share",
                  "share/{}".format(lib_name_low),
                  "obj", "src", "bin",
                  "lib", "lib/gnat"]:
            p = path.join(file_root, d)
            if not path.exists(p):
                os.mkdir(p)

        self.cache = caching.Cache(
            os.path.join(file_root, 'obj', 'langkit_cache')
        )

        # Create the project file for the generated library
        main_project_file = os.path.join(
            lib_path, "gnat",
            "{}.gpr".format(self.ada_api_settings.lib_name.lower()),
        )
        with open(main_project_file, "w") as f:
            f.write(self.render_template(
                "project_file",
                lib_name=self.ada_api_settings.lib_name,
                quex_path=os.environ["QUEX_PATH"],
            ))

        # Copy langkit_support sources files to the include prefix and
        # create its own project file.
        from os.path import dirname, abspath, join
        lngk_support_dir = join(dirname(abspath(__file__)), "support")

        for f in itertools.chain(glob(join(lngk_support_dir, "*.adb")),
                                 glob(join(lngk_support_dir, "*.ads"))):
            shutil.copy(f, join(include_path, "langkit_support"))
        shutil.copy(join(lngk_support_dir, "langkit_support_installed.gpr"),
                    join(lib_path, "gnat", "langkit_support.gpr"))

        printcol("Compiling the grammar...", Colors.OKBLUE)

        with names.camel_with_underscores:
            for r_name, r in self.grammar.rules.items():
                r.compute_fields_types()

            for r_name, r in self.grammar.rules.items():
                r.compile()
                self.rules_to_fn_names[r_name] = r

        self.order_astnode_types()

        for i, astnode in enumerate(
            (astnode
             for astnode in self.astnode_types
             if not astnode.abstract),
            # Compute kind constants for all ASTNode concrete subclasses.
            # Start with 2: the constant 0 is reserved as an
            # error/uninitialized code and the constant 1 is reserved for all
            # ASTList nodes.
            start=2
        ):
            self.node_kind_constants[astnode] = i

        with open(os.path.join(share_path, 'ast-types.txt'), 'w') as f:
            self.emit_ast_doc_txt(f)

        printcol("Generating sources... ", Colors.OKBLUE)

        with names.camel_with_underscores:
            for template_base_name, qual_name in [
                # Generate the unit for all derived AST nodes
                ("main", [self.ada_api_settings.lib_name]),
                # Generate the unit for the lexer
                ("lexer/lexer",
                 [self.ada_api_settings.lib_name, "lexer"]),
                # Generate the unit for all parsers
                ("parsers/main",
                 [self.ada_api_settings.lib_name, "parsers"]),
            ]:
                for kind_name, kind in [("spec", ada_spec),
                                        ("body", ada_body)]:
                    write_ada_file(
                        src_path, kind, qual_name,
                        self.render_template(
                            "{}_{}_ada".format(
                                template_base_name, kind_name
                            ),
                            _self=self,
                        )
                    )

            write_ada_file(
                path.join(file_root, "src"), ada_body, ["parse"],
                self.render_template("interactive_main_ada", _self=self)
            )

        with names.lower:
            # ... and the Quex C interface
            write_cpp_file(path.join(src_path, "quex_interface.h"),
                           self.render_template(
                               "lexer/quex_interface_header_c",
                               _self=self))
            write_cpp_file(path.join(src_path, "quex_interface.c"),
                           self.render_template(
                               "lexer/quex_interface_body_c",
                               _self=self))

        imain_project_file = os.path.join(file_root, "src", "parse.gpr")
        with open(imain_project_file, "w") as f:
            f.write(self.render_template(
                "parse_project_file",
                lib_name=self.ada_api_settings.lib_name,
            ))

        self.emit_c_api(src_path, include_path)
        if self.python_api_settings:
            python_path = path.join(file_root, "python")
            if not path.exists(python_path):
                os.mkdir(python_path)
            self.emit_python_api(python_path)

        # Add any sources in $lang_path/extensions/support if it exists
        if self.ext('support'):
            for f in glob(join(self.ext('support'), "*.ad*")):
                shutil.copy(f, src_path)

        printcol("Compiling the quex lexer specification", Colors.OKBLUE)

        quex_file = os.path.join(src_path,
                                 "{}.qx".format(self.lang_name.lower))
        quex_spec = self.lexer.emit()
        with open(quex_file, 'w') as f:
            f.write(quex_spec)

        # Generating the lexer C code with Quex is quite long: do it only when
        # the Quex specification changed from last build.
        if self.cache.is_stale('quex_specification', quex_spec):
            quex_py_file = path.join(environ["QUEX_PATH"], "quex-exe.py")
            subprocess.check_call([sys.executable, quex_py_file, "-i",
                                   quex_file,
                                   "-o", "quex_lexer",
                                   "--buffer-element-size", "4",
                                   "--token-id-offset",  "0x1000",
                                   "--language", "C",
                                   "--no-mode-transition-check",
                                   "--single-mode-analyzer",
                                   "--token-memory-management-by-user",
                                   "--token-policy", "single"],
                                  cwd=src_path)

        self.cache.save()

    def emit_c_api(self, src_path, include_path):
        """Generate header and binding body for the external C API."""
        def render(template_name):
            return self.render_template(template_name, _self=self)

        with names.lower:
            write_cpp_file(
                path.join(include_path,
                          "{}.h".format(self.c_api_settings.lib_name)),
                render("c_api/header_c")
            )

        with names.camel_with_underscores:
            write_ada_file(src_path, ada_spec,
                           [self.ada_api_settings.lib_name, "C"],
                           render("c_api/spec_ada"))
            write_ada_file(src_path, ada_body,
                           [self.ada_api_settings.lib_name, "C"],
                           render("c_api/body_ada"))

    def emit_python_api(self, python_path):
        """Generate the Python binding module."""
        module_filename = "{}.py".format(self.python_api_settings.module_name)

        # Collect ASTNode subclass declarations preserving "astnode_types"'s
        # order so that dependencies comes first.
        astnode_subclass_decls = []
        for cls in self.astnode_types:
            try:
                decl = self.py_astnode_subclasses[cls]
            except KeyError:
                pass
            else:
                astnode_subclass_decls.append(decl)

        with names.camel:
            with open(os.path.join(python_path, module_filename), "w") as f:
                f.write(self.render_template(
                    "python_api/module_py", _self=self,
                    c_api=self.c_api_settings,
                    pyapi=self.python_api_settings,
                    astnode_subclass_decls=astnode_subclass_decls,
                ))

    def emit_ast_doc_txt(self, file):
        """Generate a synthetic text documentation about AST nodes and types.

        :param file file: Output file for the documentation
        """
        i = 0
        for type_decl in self.enum_declarations:
            i += 1
            print >> file, 'enum {}: {}'.format(
                type_decl.type.name().camel,
                ' '.join(type_decl.type.alternatives)
            )

        if i > 0:
            print >> file, ''

        i = 0
        for typ in self.astnode_types:
            if i > 0:
                print >> file, ''
            i += 1

            # If this is not ASTNode, get the parent class
            bases = list(typ.get_inheritance_chain())
            base = bases[-2] if len(bases) > 1 else None
            fields = list(typ.get_fields())

            print >> file, '{}node {}{}{}'.format(
                'abstract ' if typ.abstract else '',
                typ.name().camel,
                '({})'.format(base.name().camel) if base else '',
                ':' if fields else ''
            )

            for field in fields:
                inherit_note = (
                    '' if field.ast_node == typ else
                    ' [inherited from {}]'.format(field.ast_node.name().camel)
                )
                print >> file, '    field {}: {}{}'.format(
                    field.name.lower, field.type.name().camel, inherit_note
                )

    @property
    def extensions_dir(self):
        """
        Returns the absolute path to the extension dir, if it exists on the
        disk, or None.
        """
        return self._extensions_dir

    @extensions_dir.setter
    def extensions_dir(self, ext_dir):
        # only set the extensions dir if this directory exists
        if os.path.isdir(ext_dir):
            self._extensions_dir = os.path.abspath(ext_dir)

    def ext(self, *args):
        """
        Return an extension file's absolute path, given strings/names
        arguments, so that you can do::

            >>> ext('a', 'b', 'c')
            $lang_dir/extensions/a/b/c

        :param [str|names.Name] args: The list of components to constitute the
                                      extension's path.

        :rtype: str
        """
        from names import Name
        args = [a.lower if isinstance(a, Name) else a for a in args]
        if self.extensions_dir:
            ret = os.path.join(self.extensions_dir, *args)
            return ret if os.path.isfile(ret) else None
