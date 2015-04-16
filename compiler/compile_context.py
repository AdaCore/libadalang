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
import tempfile

import quex_tokens
from utils import Colors


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
    """State holder for native code emission"""

    def __init__(self, lang_name, lexer_file, main_rule_name,
                 ada_api_settings, c_api_settings,
                 python_api_settings=None,
                 verbose=False):
        """Create a new context for code emission

        lang_name: Name of the target language.

        lexer_file: Filename for the Quex lexer specification.

        main_rule_name: Name for the grammar rule that will be used as an entry
        point when parsing units.

        ada_api_settings: a ada_api.AdaAPISettings instance.

        c_api_settings: a c_api.CAPISettings instance.

        python_api_settings: If provided, it must be a
        python_api.PythonAPISettings instance. In this case, a Python binding
        around the C API is generated.

        bindings: Language bindings to generate (Bindings instance).
        """
        # TODO: lang_name is not actually used anywhere at the moment

        self.lang_name = lang_name
        self.main_rule_name = main_rule_name

        self.ada_api_settings = ada_api_settings
        self.c_api_settings = c_api_settings
        self.verbose = verbose

        # Mapping: rule name -> Parser instances.
        # TODO: why do we need this? The grammar already has such a mapping.
        self.rules_to_fn_names = {}
        # Grammar instance
        self.grammar = None

        self.lexer_file = lexer_file
        quex_tokens.init_token_map(lexer_file)

        self.python_api_settings = python_api_settings

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
        self.astnode_types = None

        # Set of all ASTNode subclasses (ASTNode included) for which we
        # generate a corresponding list type.
        self.list_types = set()

        #
        # Holders for the Ada generated code chunks
        #

        # List of TypeDeclaration instances for all enumeration types used in
        # AST node fields and all ASTNode subclasses.
        self.enum_declarations = []

        # List of TypeDeclaration instances for all ASTNode subclasses
        # (including ASTList instances).
        self.types_declarations = []

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

        # Mapping: ASTNode -> list of CompiledType instances
        self.ast_fields_types = {}

    def set_ast_fields_types(self, astnode, types):
        """
        Associate `types` (a list of CompiledType) to fields in `astnode` (an
        ASTNode sub-class). It is valid to perform this association multiple
        times as long as types are consistent.
        """
        assert len(astnode.fields) == len(types), (
            "{} has {} fields ({} types given). You probably have"
            " inconsistent grammar rules and type declarations".format(
                astnode, len(astnode.fields), len(types)
            )
        )

        def is_subtype(base_type, subtype):
            return issubclass(subtype, base_type)

        def are_subtypes(base_list, new_list):
            return all(is_subtype(m, n) for m, n in zip(base_list, new_list))

        # TODO: instead of expecting types to be *exactly* the same, perform
        # type unification (take the nearest common ancestor for all field
        # types).
        assert (astnode not in self.ast_fields_types or
                are_subtypes(self.ast_fields_types[astnode], types)), (
            "Already associated types for some fields are not consistent with"
            " current ones:\n- {}\n- {}".format(
                self.ast_fields_types[astnode], types
            )
        )

        # Only assign types if astnode was not yet typed. In the case where it
        # was already typed, we checked above that the new types were
        # consistent with the already present ones
        if astnode not in self.ast_fields_types:
            self.ast_fields_types[astnode] = types

    def compute_astnode_types(self):
        """Compute the "astnode_types" field"""
        # We consider that ASTNode is not a subclass itself since it's a
        # special case for code generation. TODO: Well, actually it would be
        # cleaner to consider it, but it should be properly tagged as
        # abstract...
        from parsers import ASTNode
        self.astnode_types = [astnode
                              for astnode in self.ast_fields_types.keys()
                              if astnode != ASTNode]
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

    @property
    def render_template(self):
        # Kludge: to avoid circular dependency issues, do not import parsers
        # until needed.
        from parsers import make_renderer
        return make_renderer(self).render

    def set_grammar(self, grammar):
        self.grammar = grammar

    def emit(self, file_root="."):
        """
        Emit native code for all the rules in this grammar as a library:
        a library specification and the corresponding implementation.  Also
        emit a tiny program that can parse starting with any parsing rule for
        testing purposes.
        """
        assert self.grammar, "Set grammar before calling emit"
        assert self.lexer_file, "Set lexer before calling emit"

        lib_name_low = self.ada_api_settings.lib_name.lower()

        include_path = path.join(file_root, "include")
        src_path = path.join(file_root, "include", lib_name_low)
        lib_path = path.join(file_root, "lib")

        if not path.exists(file_root):
            os.mkdir(file_root)

        print Colors.OKBLUE + "File setup ..." + Colors.ENDC

        for d in ["include",
                  "include/liblang_support",
                  "include/{}".format(lib_name_low),
                  "obj", "src", "bin",
                  "lib", "lib/gnat"]:
            p = path.join(file_root, d)
            if not path.exists(p):
                os.mkdir(p)

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

        # Copy liblang_support sources files to the include prefix and create
        # its own project file.
        for f in itertools.chain(glob("liblang_support/*.adb"),
                                 glob("liblang_support/*.ads")):
            shutil.copy(f, path.join(include_path, "liblang_support"))
        shutil.copy("liblang_support/liblang_support_installed.gpr",
                    path.join(lib_path, "gnat", "liblang_support.gpr"))

        print Colors.OKBLUE + "Compiling the grammar ... " + Colors.ENDC

        with names.camel_with_underscores:
            for r_name, r in self.grammar.rules.items():
                r.compute_fields_types(self)

        for r_name, r in self.grammar.rules.items():
            r.compile(self)
            self.rules_to_fn_names[r_name] = r

        self.compute_astnode_types()
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

        print Colors.OKBLUE + "Generating sources ... " + Colors.ENDC

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
                            "{}_{}_ada".format(template_base_name, kind_name),
                            compile_ctx=self, _self=self,
                            token_map=quex_tokens.token_map)
                    )

            write_ada_file(path.join(file_root, "src"), ada_body, ["parse"],
                           self.render_template(
                               "interactive_main_ada", compile_ctx=self,
                               _self=self))

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

        print (
            Colors.OKBLUE + "Compiling the quex lexer specification"
            + Colors.ENDC
        )

        quex_py_file = path.join(environ["QUEX_PATH"], "quex-exe.py")

        subprocess.check_call([sys.executable, quex_py_file, "-i",
                               self.lexer_file,
                               "--engine", "quex_lexer",
                               "--token-id-offset",  "0x1000",
                               "--language", "C",
                               "--no-mode-transition-check",
                               "--single-mode-analyzer",
                               "--token-memory-management-by-user",
                               "--token-policy", "single"],
                              cwd=src_path)

    def emit_c_api(self, src_path, include_path):
        """Generate header and binding body for the external C API"""
        def render(template_name):
            return self.render_template(
                template_name, compile_ctx=self,
                _self=self,
            )

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
        """Generate the Python binding module"""
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
