from collections import defaultdict
from glob import glob
import os
import shutil
import sys
import subprocess
from os import path, environ
from distutils.spawn import find_executable
from utils import Colors
import quex_tokens


def write_cpp_file(file_path, source):
    with open(file_path, "wb") as out_file:
        if find_executable('clang-format'):
            p = subprocess.Popen(["clang-format"], stdin=subprocess.PIPE,
                                 stdout=out_file)
            p.communicate(source)
            assert p.returncode == 0
        else:
            out_file.write(source)


class CompileCtx():
    """State holder for native code emission"""

    def __init__(self, lang_name, lexer_file, main_rule_name, c_api_settings,
                 python_api_settings=None,
                 verbose=False):
        """Create a new context for code emission

        lang_name: Name of the target language.

        lexer_file: Filename for the Quex lexer specification.

        main_rule_name: Name for the grammar rule that will be used as an entry
        point when parsing units.

        c_api_settings: a c_api.CAPISettings instance.

        python_api_settings: If provided, it must be a
        python_api.PythonAPISettings instance. In this case, a Python binding
        around the C API is generated.

        bindings: Language bindings to generate (Bindings instance).
        """
        # TODO: lang_name is not actually used anywhere at the moment.
        # TODO: A short description for all these fields would help!
        self.body = []
        self.types_declarations = []
        self.types_definitions = []
        self.val_types_definitions = []
        self.fns_decls = []
        self.fns = set()
        self.generic_vectors = set()
        self.types = set()
        self.main_rule_name = main_rule_name
        self.diag_types = []
        self.test_bodies = []
        self.test_names = []
        self.rules_to_fn_names = {}
        self.grammar = None

        self.lexer_file = lexer_file
        quex_tokens.init_token_map(lexer_file)

        self.python_api_settings = python_api_settings

        # List for all ASTnode subclasses (ASTNode excluded), sorted so that A
        # is before B when A is a parent class for B. This sorting is important
        # to output declarations in dependency order.
        # This is computed right after field types inference.
        self.astnode_types = None

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

        #
        # Corresponding holders for the Python API
        #

        # Mapping: ASTNode subclass -> string (generated Python code ASTNode
        # subclass declarations).
        self.py_astnode_subclasses = {}

        # Mapping CompiledType -> string (Python declarations) for types used
        # in AST node fields.
        self.py_astnode_field_types = {}

        self.lang_name = lang_name
        self.c_api_settings = c_api_settings
        self.verbose = verbose

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
            cls: '.'.join(cls.name() for cls in cls.get_inheritance_chain())
            for cls in self.astnode_types
        }
        self.astnode_types.sort(key=lambda cls: keys[cls])

    @property
    def render_template(self):
        # Kludge: to avoid circular dependency issues, do not import parsers
        # until needed.
        from parsers import make_renderer
        return make_renderer(self).render

    def get_header(self):
        return self.render_template(
            'main_header', compile_ctx=self,
            _self=self,
            tdecls=self.types_declarations,
            tdefs=self.types_definitions,
            fndecls=self.fns_decls,
        )

    def get_source(self):
        return self.render_template(
            'main_body', compile_ctx=self,
            _self=self,
            token_map=quex_tokens.token_map,
            bodies=self.body
        )

    def get_interactive_main(self):
        return self.render_template(
            'interactive_main', compile_ctx=self,
            _self=self,
        )

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

        src_path = path.join(file_root, "src")
        include_path = path.join(file_root, "include")

        if not path.exists(file_root):
            os.mkdir(file_root)

        print Colors.OKBLUE + "File setup ..." + Colors.ENDC

        for d in ["include", "obj", "src", "bin", "lib"]:
            p = path.join(file_root, d)
            if not path.exists(p):
                os.mkdir(p)

        with open(path.join(file_root, "Makefile"), "w") as f:
            f.write(self.render_template(
                "Makefile",
                compile_ctx=self,
            ))

        # These are internal headers, so they go to "src". Only external ones
        # headers (for the public API) go to "include" (see below).
        for f in glob("support/*.hpp"):
            shutil.copy(f, src_path)

        for f in glob("support/*.cpp"):
            shutil.copy(f, src_path)

        print Colors.OKBLUE + "Compiling the grammar ... " + Colors.ENDC

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
            # Start with 1: the constant 0 is reserved for all ASTList nodes.
            start=1
        ):
            self.node_kind_constants[astnode] = i

        write_cpp_file(path.join(src_path, "parse.cpp"),
                       self.get_source())

        write_cpp_file(path.join(src_path, "parse.hpp"),
                       self.get_header())

        write_cpp_file(
            path.join(src_path, "parse_main.cpp"),
            self.get_interactive_main()
        )

        write_cpp_file(
            path.join(src_path, "ast.hpp"),
            self.render_template("ast_header", compile_ctx=self),
        )

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
                              cwd=path.join(file_root, "src"))

    def emit_c_api(self, src_path, include_path):
        """Generate header and binding body for the external C API"""
        def render(template_name):
            return self.render_template(
                template_name, compile_ctx=self,
                _self=self,
            )

        write_cpp_file(
            path.join(include_path,
                      "{}.h".format(self.c_api_settings.lib_name)),
            render("c_header")
        )
        write_cpp_file(
            path.join(src_path,
                      "{}.cpp".format(self.c_api_settings.lib_name)),
            render("c_body")
        )

        write_cpp_file(
            path.join(src_path, "c_utils.hpp"),
            render("c_utils")
        )

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

        with open(os.path.join(python_path, module_filename), "w") as f:
            f.write(self.render_template(
                'python/module', _self=self,
                pyapi=self.python_api_settings,
                astnode_subclass_decls=astnode_subclass_decls,
            ))
